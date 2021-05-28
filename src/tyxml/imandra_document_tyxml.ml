

module D = Imandra_document.Document
module H = Tyxml.Html

type 'a div_html = ([> Html_types.div] as 'a) H.elt

let cast_to_html (h:D.html) : _ div_html =
  (H.Unsafe.data (h:D.html :> string) : [>`Div] H.elt)

let to_doc (h: [`Div]div_html) : D.t =
  let s = Format.asprintf "%a@." (Tyxml.Html.pp_elt()) h in
  D.html (D.Unsafe_.html_of_string s)

module Mapper = struct
  type doc = D.t

  type t = {
    to_doc: t -> depth:int -> doc -> Html_types.div_content_fun H.elt;
    attr_header: t -> col:int -> string -> Html_types.th_attrib H.attrib list;
    attr_row: t -> row:int -> col:int -> doc -> Html_types.td_attrib H.attrib list;
  }

  let mk_header ?a ~depth l : _ div_html = match depth with
    | 1 -> H.h1 ?a l
    | 2 -> H.h2 ?a l
    | 3 -> H.h3 ?a l
    | 4 -> H.h4 ?a l
    | 5 -> H.h5 ?a l
    | n when n>=6 -> H.h6 ?a l
    | _ -> assert false

  let default : t = {
    attr_header = (fun _self ~col:_ _s -> [H.a_class ["col"; "m-1"; "p-1"]]);

    attr_row = (fun _self ~row:_ ~col:_ _d -> [H.a_class ["col"; "m-1"; "p-1"]]);

    to_doc = fun self ~depth doc ->
      let recur ~depth d =
        self.to_doc self ~depth d
      in

      let open D in

      (* obtain HTML attributes *)
      let a =
        CCList.filter_map
          (function
            | A_color s -> Some (H.a_style ("color:"^s))
            | A_class s -> Some (H.a_class [s])
            | A_custom _ -> None)
          (attrs doc)
      in

      let rec render_doc ~a ~depth doc =
        match view doc with
        | String s ->
          let h = H.txt s in
          if a=[] then h else H.span ~a [h]

        | Text s -> H.p ~a [H.txt s]

        | Section (s,l) ->
          H.div
            (mk_header ~a ~depth [H.txt s] ::
             List.map (render_doc ~a ~depth:(depth+1)) l)

        | Pre s -> H.pre ~a [H.txt s]

        | List {l;_} ->
          H.ul ~a:(H.a_class ["list-group"] :: a)
            (List.map (fun sub -> H.li ~a:[H.a_class ["list-group-item"]]
                          [recur ~depth sub]) l)

        | Block l ->
          H.div ~a:(H.a_class ["container"]::a) (List.map (recur ~depth) l)

        | V_block l ->
          (* insert paragraphs for skipping lines *)
          H.div ~a (CCList.flat_map (fun d -> [recur ~depth d; H.p []]) l)

        | Indented (s,sub) ->
          H.div ~a [
            mk_header ~a ~depth [H.txt s];
            recur ~depth sub;
          ]

        | Tbl {headers;rows} ->
          let hd =
            headers
            |> CCOpt.map (fun row ->
                H.thead [
                  H.tr ~a:[H.a_class ["row"]] @@
                  List.mapi
                    (fun i s ->
                       let a = self.attr_header self ~col:i s in
                       H.th ~a [H.txt s])
                    row
                ])
          and rows =
            rows
            |> List.mapi (fun i_row row ->
                H.tr ~a:[H.a_class ["row"]] @@
                List.mapi
                  (fun i_col s ->
                     let a = self.attr_row self ~row:i_row ~col:i_col s in
                     H.td ~a [recur ~depth s])
                  row
              )
          in
          H.div ~a:[H.a_class ["container"]] [
            H.table ?thead:hd ~a:[H.a_class ["table"]] rows
          ]

        | Record l ->
          (* convert to table on the fly *)
          let d = D.tbl_of_rows
              (fun (k,v) -> [D.s_f ~a:[A.cls "col-2"] "%s:" k; v]) l in
          render_doc ~depth ~a d

        | Graphviz s ->
          H.div [
            H.h4 [H.txt "graphviz"];
            H.pre [H.txt s];
          ]
          (* TODO
          let id = "graphviz-" ^ (Uuidm.v `V4 |> Uuidm.to_string) in
          H.div ~a:[H.a_class ["imandra-graphviz"]; H.a_id id]
            [ H.textarea ~a:[H.a_style "display: none"] (H.txt s)
            ; H.button ~a:[H.a_class ["btn"; "btn-primary"]] [(H.txt "Load graph")]
            ; H.div ~a:[H.a_class ["imandra-graphviz-loading"; "display-none"]] [(H.txt "Loading..")]
            ; H.div ~a:[H.a_class ["imandra-graphviz-target"]] []
            ; H.txt  (H.Unsafe.data (graphviz_js id))
            ]
             *)

        | Enum l ->
          H.ol ~a:(H.a_class ["list-group"] :: a)
            (List.map (fun sub -> H.li ~a:(H.a_class["list-group-item"]::a)
                          [recur ~depth sub]) l)

        | Bold d -> H.b ~a [H.txt @@ to_string d]
        | Italic d -> H.i ~a [H.txt @@ to_string d]
        | Url {url;txt} -> H.a ~a:[H.a_href url] [H.txt txt]
        | OCamldoc_ref _
        | OCamldoc_tag _ -> H.txt @@ to_string doc

        | Fold { folded_by_default; summary=sum; sub=d' } ->
          H.details
            (H.summary [H.txt sum])
            ~a:((if folded_by_default then [] else [H.a_open()])@[H.a_class["summary"]])
            [recur ~depth d']

        | Alternatives {views=vs; _} ->
          (* TODO : do better than a list. maybe a web component for tabs,
             defined in a prelude above? *)
          H.div [recur ~depth @@ v_block @@
               List.map (fun (s,x) -> fold ~summary:s ~folded_by_default:true x) vs
              ]

        | Regions rs ->
          let aux_region {constraints;invariant} =
            H.div ~a:[H.a_class ["container"; "border"; "border-primary"]] [
              H.h4 [H.txt "constraints"];
              H.ul ~a:[H.a_class ["container"; "list-group"]] @@
              List.map (fun x ->
                  H.li ~a:[H.a_class ["list-group-item"; "m-1"; "p-1"]] [H.pre[H.txt x]])
                constraints;
              H.h4 [H.txt "invariant"];
              H.pre [H.txt invariant];
            ]
          in
          H.div ~a:[H.a_class ["container"]] [
            H.h3 [H.txt "regions"];
            H.ul ~a:[H.a_class ["list-group"]] @@
            List.map (fun r ->
                H.li ~a:[H.a_class ["list-group-item"]] [aux_region r])
              rs
          ]

        | Html h -> H.div [cast_to_html h]

      in

      render_doc ~a ~depth doc
  }

  let run_elt (self:t) (doc:D.t) : _ H.elt =
    H.div [self.to_doc self ~depth:1 doc]

  let run_doc
      ?(title="doc") ?meta:(my_meta=[])
      ?(headers : Html_types.head_content_fun H.elt list =[])
      (self:t) (doc:D.t) : H.doc =
    let style0 =
      let l = [
        "table.framed { border: 2px solid black; }";
        "table.framed th, table.framed td { border: 1px solid black; }";
        "th, td { padding: 3px; }";
      ] in
      H.style (CCList.map H.txt l)
    in
    let b_style = H.link ~rel:[`Stylesheet]
        ~href:"https://maxcdn.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css" ()
    in
    H.html
      (H.head (H.title @@ H.txt title) @@ [
          style0;
          b_style;
          H.meta ~a:(H.a_charset "utf-8" :: my_meta) ();
        ] @ headers)
      (H.body [
          H.div ~a:[H.a_class ["container"]] [run_elt self doc]
        ])
end

(* adapted from jymandra *)
let to_html_elt (doc:D.t) : _ H.elt =
  Mapper.run_elt Mapper.default doc

let to_html_doc ?title ?meta ?headers doc =
  Mapper.run_doc ?title ?meta ?headers Mapper.default doc

let string_of_html_doc d : string =
  Format.asprintf "%a@." (Tyxml.Html.pp()) d

let string_of_html_elt d : string =
  Format.asprintf "%a@." (Tyxml.Html.pp_elt()) d

let to_string_html_elt d : string = string_of_html_elt @@ to_html_elt d

let to_string_html_doc ?title ?meta ?headers d : string =
  string_of_html_doc @@ to_html_doc ?title ?meta ?headers d
