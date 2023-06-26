module Document = Imandra_document.Document

module Encode (E : Decoders.Encode.S) = struct
  open E

  let attribute (attr : Document.attribute) =
    match attr with
    | A_color s -> obj [ ("color", string s) ]
    | A_class s -> obj [ ("class", string s) ]
    | A_custom (ty, s) -> obj [ ("custom", list value [ string ty; string s ]) ]

  let attributes (attrs : Document.attributes) = list attribute attrs

  let region (region : Document.region) =
    obj
      [
        ("constraints", list string region.constraints);
        ("invariant", list string region.invariant);
      ]

  let rec t (doc : Document.t) =
    obj
      [
        ("attributes", attributes (Document.attrs doc));
        ("view", view (Document.view doc));
      ]

  and view (view : Document.view) =
    let obj_ty ty kvs = obj (("ty", string ty) :: kvs) in
    match view with
    | Section (title, children) ->
        obj_ty "section"
          [ ("title", string title); ("children", list t children) ]
    | String s -> obj_ty "string" [ ("string", string s) ]
    | Text s -> obj_ty "text" [ ("text", string s) ]
    | Pre s -> obj_ty "pre" [ ("pre", string s) ]
    | Block children -> obj_ty "block" [ ("children", list t children) ]
    | V_block children -> obj_ty "v_block" [ ("children", list t children) ]
    | Indented (title, children) ->
        obj_ty "indented"
          [ ("title", string title); ("children", list t children) ]
    | List { bullet; l } ->
        obj_ty "list" [ ("bullet", string bullet); ("children", list t l) ]
    | Tbl { headers : string list option; rows } ->
        obj_ty "tbl"
          (List.concat
             [
               (match headers with
               | Some headers -> [ ("headers", list string headers) ]
               | None -> []);
               [ ("rows", (list (list t)) rows) ];
             ])
    | Graphviz s -> obj_ty "graphviz" [ ("g", string s) ]
    | Enum children -> obj_ty "enum" [ ("children", list t children) ]
    | Bold child -> obj_ty "bold" [ ("child", t child) ]
    | Italic child -> obj_ty "italic" [ ("child", t child) ]
    | Url { url; txt } ->
        obj_ty "url" [ ("url", string url); ("txt", string txt) ]
    | OCamldoc_ref s -> obj_ty "ocamldoc_ref" [ ("ref", string s) ]
    | OCamldoc_tag s -> obj_ty "ocamldoc_tag" [ ("tag", string s) ]
    | Fold { folded_by_default; summary; sub } ->
        obj_ty "fold"
          [
            ("folded_by_default", bool folded_by_default);
            ("summary", string summary);
            ("child", t sub);
          ]
    | Alternatives { views } ->
        obj_ty "alternatives"
          [ ("views", obj (List.map (fun (k, v) -> (k, t v)) views)) ]
    | Regions regions -> obj_ty "regions" [ ("regions", list region regions) ]
    | Html html -> obj_ty "html" [ ("html", string html) ]
    | Record items ->
        obj_ty "record"
          [ ("record", obj (List.map (fun (k, v) -> (k, t v)) items)) ]
end
