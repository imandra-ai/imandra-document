module Document = Imandra_document.Document

module Encode (E : Decoders.Encode.S) = struct
  open E

  let attribute (attr : Document.attribute) =
    let obj_ty ty kvs = obj (("ty", string ty) :: kvs) in
    match attr with
    | A_color s -> obj_ty "color" [ ("value", string s) ]
    | A_class s -> obj_ty "class" [ ("value", string s) ]
    | A_custom (ty, s) ->
        obj_ty "custom" [ ("custom_ty", string ty); ("value", string s) ]

  let attributes (attrs : Document.attributes) = list attribute attrs

  let region (region : Document.region) =
    obj
      [
        ("constraints", list string region.constraints);
        ("invariant", string region.invariant);
      ]

  let ocamldoc_see_ref (see : Document.ocamldoc_see_ref) =
    let obj_ty ty kvs = obj (("ty", string ty) :: kvs) in
    match see with
    | See_url s -> obj_ty "url" [ ("value", string s) ]
    | See_file s -> obj_ty "file" [ ("value", string s) ]
    | See_doc s -> obj_ty "doc" [ ("value", string s) ]

  let rec t (doc : Document.t) =
    obj
      (("attributes", attributes (Document.attrs doc))
      :: view (Document.view doc))

  and view (view : Document.view) =
    let with_ty ty kvs = ("ty", string ty) :: kvs in
    match view with
    | Section (title, children) ->
        with_ty "section"
          [ ("title", string title); ("children", list t children) ]
    | String s -> with_ty "string" [ ("string", string s) ]
    | Text s -> with_ty "text" [ ("text", string s) ]
    | Pre s -> with_ty "pre" [ ("pre", string s) ]
    | Block children -> with_ty "block" [ ("children", list t children) ]
    | V_block children -> with_ty "v_block" [ ("children", list t children) ]
    | Indented (title, child) ->
        with_ty "indented" [ ("title", string title); ("child", t child) ]
    | List { bullet; l } ->
        with_ty "list" [ ("bullet", string bullet); ("children", list t l) ]
    | Tbl { headers : string list option; rows } ->
        with_ty "tbl"
          (List.concat
             [
               (match headers with
               | Some headers -> [ ("headers", list string headers) ]
               | None -> []);
               [ ("rows", (list (list t)) rows) ];
             ])
    | Graphviz s -> with_ty "graphviz" [ ("g", string s) ]
    | Enum children -> with_ty "enum" [ ("children", list t children) ]
    | Bold child -> with_ty "bold" [ ("child", t child) ]
    | Italic child -> with_ty "italic" [ ("child", t child) ]
    | Url { url; txt } ->
        with_ty "url" [ ("url", string url); ("txt", string txt) ]
    | OCamldoc_ref s -> with_ty "ocamldoc_ref" [ ("ref", string s) ]
    | OCamldoc_tag tag -> with_ty "ocamldoc_tag" [ ("tag", ocamldoc_tag t tag) ]
    | Fold { folded_by_default; summary; sub } ->
        with_ty "fold"
          [
            ("folded_by_default", bool folded_by_default);
            ("summary", string summary);
            ("child", t sub);
          ]
    | Alternatives { views } ->
        let view (name, child) =
          obj [ ("name", string name); ("child", t child) ]
        in
        with_ty "alternatives" [ ("views", list view views) ]
    | Regions regions -> with_ty "regions" [ ("regions", list region regions) ]
    | Html _html -> with_ty "html" [ ("html", string "<NOT IMPLEMENTED>") ]
    | Record items ->
        let item (key, value) =
          obj [ ("key", string key); ("value", t value) ]
        in
        with_ty "record" [ ("record", list item items) ]

  and ocamldoc_tag (t : Document.t encoder) (tag : Document.ocamldoc_tag) =
    let obj_ty ty kvs = obj (("ty", string ty) :: kvs) in
    match tag with
    | OT_author s -> obj_ty "author" [ ("author", string s) ]
    | OT_version s -> obj_ty "version" [ ("version", string s) ]
    | OT_see (see_ref, doc) ->
        obj_ty "see" [ ("ref", ocamldoc_see_ref see_ref); ("doc", t doc) ]
    | OT_since s -> obj_ty "since" [ ("since", string s) ]
    | OT_before (s, doc) ->
        obj_ty "before" [ ("before", string s); ("doc", t doc) ]
    | OT_deprecated doc -> obj_ty "deprecated" [ ("doc", t doc) ]
    | OT_param (s, doc) ->
        obj_ty "param" [ ("param", string s); ("doc", t doc) ]
    | OT_raised_exception (s, doc) ->
        obj_ty "raised_exception"
          [ ("raised_exception", string s); ("doc", t doc) ]
    | OT_return_value doc -> obj_ty "return_value" [ ("doc", t doc) ]
    | OT_inline -> obj_ty "inline" []
    | OT_custom (s, doc) ->
        obj_ty "custom" [ ("custom", string s); ("doc", t doc) ]
    | OT_canonical s -> obj_ty "canonical" [ ("canonical", string s) ]
end

module Decode (D : Decoders.Decode.S) = struct
  open D

  let attribute : Document.attribute decoder =
    field "ty" string >>= function
    | "color" -> field "value" string >|= fun value -> Document.A_color value
    | "class" -> field "value" string >|= fun value -> Document.A_class value
    | "custom" ->
        field "custom_ty" string >>= fun ty ->
        field "value" string >>= fun value ->
        succeed (Document.A_custom (ty, value))
    | _ -> fail "unknown attribute type"

  let attributes : Document.attributes decoder = list attribute

  let region : Document.region decoder =
    field "constraints" (list string) >>= fun constraints ->
    field "invariant" string >>= fun invariant ->
    succeed { Document.constraints; invariant }

  let ocamldoc_see_ref : Document.ocamldoc_see_ref decoder =
    field "ty" string >>= function
    | "url" -> field "value" string >|= fun url -> Document.See_url url
    | "file" -> field "value" string >|= fun file -> Document.See_file file
    | "doc" -> field "value" string >|= fun doc -> Document.See_doc doc
    | _ -> fail "unknown ocamldoc_see_ref type"

  let ocamldoc_tag (t : Document.t decoder) : Document.ocamldoc_tag decoder =
    field "ty" string >>= function
    | "author" -> field "author" string >|= fun s -> Document.OT_author s
    | "version" -> field "version" string >|= fun s -> Document.OT_version s
    | "see" ->
        field "ref" ocamldoc_see_ref >>= fun see_ref ->
        field "doc" t >>= fun doc -> succeed (Document.OT_see (see_ref, doc))
    | "since" -> field "since" string >|= fun s -> Document.OT_since s
    | "before" ->
        field "before" string >>= fun before ->
        field "doc" t >>= fun doc -> succeed (Document.OT_before (before, doc))
    | "deprecated" ->
        field "doc" t >>= fun doc -> succeed (Document.OT_deprecated doc)
    | "param" ->
        field "param" string >>= fun param ->
        field "doc" t >>= fun doc -> succeed (Document.OT_param (param, doc))
    | "raised_exception" ->
        field "raised_exception" string >>= fun raised_exception ->
        field "doc" t >>= fun doc ->
        succeed (Document.OT_raised_exception (raised_exception, doc))
    | "return_value" ->
        field "doc" t >>= fun doc -> succeed (Document.OT_return_value doc)
    | "inline" -> succeed Document.OT_inline
    | "custom" ->
        field "custom" string >>= fun custom ->
        field "doc" t >>= fun doc -> succeed (Document.OT_custom (custom, doc))
    | "canonical" ->
        field "canonical" string >>= fun canonical ->
        succeed (Document.OT_canonical canonical)
    | _ -> fail "unknown ocamldoc_tag type"

  let view (t : Document.t decoder) (a : Document.attributes) =
    field "ty" string >>= function
    | "section" ->
        field "title" string >>= fun title ->
        field "children" (list t) >>= fun children ->
        succeed (Document.section ~a title children)
    | "string" ->
        field "string" string >>= fun string -> succeed (Document.s ~a string)
    | "text" ->
        field "text" string >>= fun text -> succeed (Document.paragraph ~a text)
    | "pre" -> field "pre" string >>= fun pre -> succeed (Document.pre ~a pre)
    | "block" ->
        field "children" (list t) >>= fun children ->
        succeed (Document.block ~a children)
    | "v_block" ->
        field "children" (list t) >>= fun children ->
        succeed (Document.v_block ~a children)
    | "indented" ->
        field "title" string >>= fun title ->
        field "child" t >>= fun child ->
        succeed (Document.indent ~a title child)
    | "list" ->
        field "bullet" string >>= fun bullet ->
        field "children" (list t) >>= fun children ->
        succeed (Document.list ~a ~bullet children)
    | "tbl" ->
        field_opt "headers" (list string) >>= fun headers ->
        field "rows" (list (list t)) >>= fun rows ->
        succeed (Document.tbl ~a ?headers rows)
    | "graphviz" ->
        field "g" string >>= fun g -> succeed (Document.graphviz ~a g)
    | "enum" ->
        field "children" (list t) >>= fun children ->
        succeed (Document.enum ~a children)
    | "bold" ->
        field "child" t >>= fun child -> succeed (Document.bold ~a child)
    | "italic" ->
        field "child" t >>= fun child -> succeed (Document.italic ~a child)
    | "url" ->
        field "url" string >>= fun url ->
        field "txt" string >>= fun txt -> succeed (Document.url ~a ~url txt)
    | "ocamldoc_ref" ->
        field "ref" string >>= fun ref -> succeed (Document.ref ~a ref)
    | "ocamldoc_tag" ->
        field "tag" (ocamldoc_tag t) >>= fun tag ->
        succeed (Document.tag ~a tag)
    | "fold" ->
        field "folded_by_default" bool >>= fun folded_by_default ->
        field "summary" string >>= fun summary ->
        field "child" t >>= fun child ->
        succeed (Document.fold ~a ~folded_by_default ~summary child)
    | "alternatives" ->
        let view =
          field "name" string >>= fun name ->
          field "child" t >>= fun child -> succeed (name, child)
        in
        field "views" (list view) >>= fun views ->
        succeed (Document.alternatives ~a views)
    | "regions" ->
        field "regions" (list region) >>= fun regions ->
        succeed (Document.regions regions)
    | "html" ->
        field "html" string >>= fun html ->
        succeed (Document.html (Document.Unsafe_.html_of_string html))
    | "record" ->
        let item =
          field "key" string >>= fun key ->
          field "value" t >>= fun value -> succeed (key, value)
        in
        field "record" (list item) >>= fun items ->
        succeed (Document.record ~a items)
    | _ -> fail "unknown document type"

  let t : Document.t decoder =
    fix (fun t -> field "attributes" attributes >>= fun attrs -> view t attrs)
end
