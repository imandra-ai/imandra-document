

(* This file is free software. See file "license" for more details. *)

(** {1 Simple Formatted Document} *)

module Fmt = CCFormat

type attribute =
  | A_color of string
  | A_class of string
  | A_custom of string * string

type attributes = attribute list

type region =
  { constraints : string list;
    invariant : string }

type view =
  | Section of string (* section *)
  | String of string (** raw string *)
  | Text of string (** text with flexible newlines and spaces *)
  | Pre of string (* pre-formatted paragraph *)
  | Block of t list
  | V_block of t list (* list of docs, separated by empty lines *)
  | Indented of string * t (** indented block with header *)
  | List of {
      bullet: string;
      l: t list; (** list of items *)
    }
  | Tbl of {
      headers: string list option;
      rows: t list list;
    } (** table *)
  | Graphviz of string (** graphviz code for an inline graph *)
  | Enum of t list (** numbered list *)
  | Bold of t
  | Italic of t
  | Url of {
      url: string;
      txt: string;
    }
  | OCamldoc_ref of string
  | OCamldoc_tag of ocamldoc_tag
  | Fold of {
      folded_by_default: bool; (* is it originally folded? *)
      summary: string; (* what to print when it's folded *)
      sub: t;
    } (** A foldable document, with a short summary *)
  | Alternatives of {
      views: (string * t) list; (** possible ways of representing the same thing *)
    }
  | Regions of region list
  | Html of [`Div] Tyxml.Html.elt

and ocamldoc_see_ref =
  | See_url of string
  | See_file of string
  | See_doc of string

(** Tags *)
and ocamldoc_tag =
  | OT_author of string (** \@author tag *)
  | OT_version of string (** \@version tag *)
  | OT_see of ocamldoc_see_ref * t (** \@see tag *)
  | OT_since of string (** \@since tag *)
  | OT_before of string * t (** \@before tag *)
  | OT_deprecated of t (** \@deprecated tag *)
  | OT_param of string * t (** \@param tag *)
  | OT_raised_exception of string * t (** \@raise tag *)
  | OT_return_value of t (** \@return tag *)
  | OT_inline (** \@inline tag *)
  | OT_custom of string * t (** custom tag *)
  | OT_canonical of string (** \@canonical tag *)

and t = {
  view: view;
  attrs: attributes;
}

type doc = t

let view t = t.view
let attrs t = t.attrs

let ksprintf ~f fmt =
  let buf = Buffer.create 32 in
  let out = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun _ -> Format.pp_print_flush out (); f (Buffer.contents buf))
    out fmt

let mk_ ?(a=[]) v = {view=v; attrs=a}

let block ?a l = mk_ ?a @@ Block l
let v_block ?a l = mk_ ?a @@ V_block l
let empty = block[]
let block_of ?a f l = block ?a (List.map f l)
let v_block_of ?a f l = v_block ?a (List.map f l)
let section ?a s = mk_ ?a @@ Section s
let section_f ?a s = ksprintf ~f:(section ?a) s
let paragraph ?a s = mk_ ?a @@ Text s
let paragraph_f ?a s = ksprintf ~f:(paragraph ?a) s
let s str = mk_ @@ String str
let s_f str = ksprintf ~f:s str
let int i = s (string_of_int i)
let bigint x = s (Z.to_string x)
let rat x = s (Q.to_string x)
let p ?a s = paragraph ?a s
let p_f ?a s = ksprintf ~f:(p ?a) s
let indent ?a i j = mk_ ?a @@ Indented (i,j)
let pre ?a s = mk_ ?a @@ Pre s
let pre_f ?a s = ksprintf ~f:(pre ?a) s
let list ?a ?(bullet="- ") l = mk_ ?a @@ List {l; bullet}
let list1 ?a ?bullet = function
  | [x] -> x
  | l -> list ?a ?bullet l
let list_of ?a f l = list ?a (List.map f l)
let list_of1 ?a f l = list1 ?a (List.map f l)
let enum ?a l = mk_ ?a @@ Enum l
let tbl ?a ?headers rows = mk_ ?a @@ Tbl {headers; rows}
let tbl_of ?a ?headers f l = tbl ?a ?headers @@ List.map (List.map f) l
let tbl_of_rows ?a ?headers f l = tbl ?a ?headers @@ List.map f l
let graphviz ?a s = mk_ ?a @@ Graphviz s
let verbatim txt = s txt
let bold d = mk_ @@ Bold d
let tag ?a tag = mk_ ?a @@ OCamldoc_tag tag
let ref ?a s = mk_ ?a @@ OCamldoc_ref s
let url ?a ~url txt = mk_ ?a @@ Url {url;txt}
let fold ?a ?(folded_by_default=true) ?(summary="") sub =
  mk_ ?a @@ Fold {folded_by_default;summary;sub}
let alternatives ?a l =
  match l with
  | [] -> empty
  | [_,x] -> x
  | _ -> mk_ ?a @@ Alternatives {views=l}
let regions l =
  mk_ @@ Regions l
let html s =
  mk_ @@ Html s

let record ?a l = tbl_of_rows ?a (fun (k,v) -> [bold (s k); v]) l

let intersperse sep l =
  List.fold_left
    (fun acc x ->
       if acc=[] then [x] else x::sep::acc)
    [] l
  |> List.rev

let some = function None -> empty | Some d -> d
let map_opt f = function None -> empty | Some x -> f x

module A = struct
  type t = attribute

  let color s = A_color s

  let red = color "red"
  let green = color "green"
  let blue = color "blue"
  let yellow = color "yellow"

  let cls c = A_class c
end

let pp_list_ p =
  Format.pp_print_list ~pp_sep:(fun out () -> Format.pp_print_cut out ()) p
let pp_list_sp_ p =
  Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "@ ") p

let rec pp out (d:t) : unit =
  (* print within colors? *)
  match CCList.find_map (function A_color s -> Some s | _ -> None) d.attrs with
  | None -> pp_content out d
  | Some c ->
    Fmt.fprintf out "@{<%s>%a@}" c pp_content d
and pp' out d = Fmt.fprintf out "@[%a@]" pp d
and pp_content out d = match view d with
  | Section sec -> Fmt.fprintf out "@{<Blue>@[<h>%s@]@}" sec
  | String msg -> Fmt.string out msg
  | Text msg -> Format.fprintf out "@[%a@]" Format.pp_print_text msg
  | Pre msg when String.contains msg '\n' ->
    (* code-block *)
    Fmt.fprintf out "`@[<v>";
    String.iter
      (function
        | '\n' -> Format.fprintf out "@,"
        | c -> Format.pp_print_char out c)
      msg;
    Fmt.fprintf out "@]`"
  | Pre msg -> Fmt.fprintf out "`%s`" msg
  | Indented (head, body) ->
    Fmt.fprintf out "@[<v2>%s::@ %a@]" head pp body
  | Block l -> Fmt.fprintf out "@[%a@]" (pp_list_ pp) l
  | V_block l ->
    (* vertical block separated by empty lines *)
    Fmt.fprintf out "@[<v>%a@]" (Fmt.list ~sep:(Fmt.return "@,@,") pp) l
  | List {l;bullet} ->
    let pp_item out x = Fmt.fprintf out "@[<2>%s@[%a@]@]" bullet pp x in
    Fmt.fprintf out "@[<v>%a@]" (pp_list_ pp_item) l
  | Tbl {headers;rows} ->
    let li = String.make 76 '-' in
    let pp_li out () = Format.pp_print_string out li in
    let pp_row out r = Format.fprintf out "@[<hv>%a@]" (pp_list_sp_ pp') r in
    begin match headers with
      | Some hds ->
        Format.fprintf out "{@[<v>%a@,%a" (pp_list_ Format.pp_print_string) hds pp_li ();
      | None -> Format.fprintf out "{@[<v>";
    end;
    List.iteri
      (fun i row ->
         if i=0 then pp_row out row
         else Format.fprintf out "@,%a@,%a" pp_li () pp_row row)
      rows;
    Format.fprintf out "@]}"
  | Graphviz _ -> Fmt.string out "<graph>"
  | Enum l ->
    let n = Stdlib.ref 0 in
    let pp_item out x : unit = incr n; Fmt.fprintf out "@[<2>%d. @[%a@]@]" !n pp x in
    Fmt.fprintf out "@[<v>%a@]" (pp_list_ pp_item) l
  | Bold t ->
    Fmt.fprintf out "@{<bold>%a@}" pp t
  | Italic t -> pp out t (* no way of rendering this *)
  | Url {url; txt} -> Fmt.fprintf out "[%s](@{<green>%s@})" txt url (* markdown style *)
  | OCamldoc_tag tag -> pp_ocamldoc_tag out tag
  | OCamldoc_ref s -> Fmt.fprintf out "{!%s}" s
  | Fold {sub;_} -> pp out sub
  | Alternatives {views=l} ->
    let pp_alt out (s,d) = Fmt.fprintf out "(@[%s:@ %a@])" s pp d in
    Fmt.fprintf out "(@[<hv>alternatives@ %a@])"
      Fmt.(list ~sep:(return "@ ") pp_alt) l
  | Regions rgs ->
    let pp_region out {constraints;invariant} =
      Fmt.fprintf out
       "@[---[region]---@\n\
        @[Constraints:@\n\
        @ @[[@[%a@]]@]@\n\
        @[Invariant:@\n\
        @ @[%s@]@\n\
        -------------@]"
       CCFormat.(list ~sep:(return ";@\n") string) constraints
       invariant
    in
    Fmt.fprintf out "(@[<v>Regions@ %a@])"
      Fmt.(list ~sep:(return "@\n") pp_region) rgs
  | Html html -> Format.fprintf out "@[%a@]" (Tyxml.Html.pp_elt ()) html

and pp_ocamldoc_tag out = function
  | OT_canonical s -> Fmt.fprintf out "@@canonical %s" s
  | OT_author s -> Fmt.fprintf out "@@author %s" s
  | OT_version s -> Fmt.fprintf out "@@version %s" s
  | OT_see (sr,s) -> Fmt.fprintf out "@[<2>%a@ %a@]" pp_ocamldoc_see_ref sr pp s
  | OT_since s -> Fmt.fprintf out "@@since %s" s
  | OT_before (s,d) -> Fmt.fprintf out "@[<2>@@before %s@ %a@]" s pp d
  | OT_deprecated d -> Fmt.fprintf out "@[<2>@@deprecated@ %a@]" pp d
  | OT_param (s,d) -> Fmt.fprintf out "@[<2>@@param %s@ %a@]" s pp d
  | OT_raised_exception (s,d) -> Fmt.fprintf out "@[<2>@@raises %s@ %a@]" s pp d
  | OT_return_value d -> Fmt.fprintf out "@[<2>@@return@ %a@])" pp d
  | OT_inline -> Fmt.string out "@inline"
  | OT_custom (s,d) -> Fmt.fprintf out "@[<2>@@%s@ %a@]" s pp d

and pp_ocamldoc_see_ref out = function
  | See_url url -> Fmt.fprintf out "@@see %S" url
  | See_file f -> Fmt.fprintf out "@@see file %S" f
  | See_doc d -> Fmt.fprintf out "@@see %s" d

let to_string = CCFormat.to_string pp

(** {2 Graph Builder} *)
module Graph = struct
  type event =
    | Set_graph_name of string
    | Node of {name: string; label: string}
    | Edge of {src: string; target: string; label: string}

  type t = event list

  let n ?lbl name = Node {name; label=CCOpt.get_or ~default:name lbl}
  let e ?lbl src target = Edge {src;target; label=CCOpt.get_or ~default:"" lbl}
  let set_graph_name s = Set_graph_name s

  let default_style_dot = "fontname=\"courier\",fontsize=14"
  let escape_dot s =
    let b = Buffer.create (String.length s + 5) in
    String.iter
      (fun c ->
         begin match c with
           | '|' | '\\' | '{' | '}' | '<' | '>' | '"' ->
             Buffer.add_char b '\\'; Buffer.add_char b c
           | '\n' -> Buffer.add_string b "\\l"; (* left justify *)
           | _ -> Buffer.add_char b c
         end)
      s;
    Buffer.contents b

  let to_doc ?a (g:t) : doc =
    let buf = Buffer.create 42 in
    let name =
      match CCList.find_map (function Set_graph_name s -> Some s | _ -> None) g with
      | None -> "graph"
      | Some s -> s
    in
    Printf.bprintf buf "digraph \"%s\" {\n" name;
    List.iter
      (function
        | Set_graph_name _ -> ()
        | Node {name; label} ->
          Printf.bprintf buf "%s [label=\"%s\",%s];\n" name
            (escape_dot label) default_style_dot
        | Edge {src;target;label} ->
          Printf.bprintf buf "%s -> %s [label=\"%s\",%s];\n"
            src target (escape_dot label) default_style_dot

      ) g;
    Printf.bprintf buf "\n}\n";
    graphviz ?a (Buffer.contents buf)
end

(** {2 Parsing} *)

let of_octavius (d:Octavius.Types.t) : t =
  let module T = Octavius.Types in
  let rec aux_t (txt,tags) =
    let doc = aux_txt txt in
    let tags = List.map aux_tag tags in
    match tags with
    | [] -> doc
    | _ -> block (doc :: tags)
  and aux_txt (t:T.text) : t =
    (* split into paragraphs *)
    let rec group_paragraphs acc cur_para = function
      | [] -> List.rev (List.rev cur_para :: acc)
      | T.Newline :: tail ->
        (* new paragraph *)
        group_paragraphs (List.rev cur_para :: acc) [] tail
      | x :: tail ->
        let x = aux_txt_elt x in
        group_paragraphs acc (x::cur_para) tail
    in
    match group_paragraphs [] [] t with
    | [] -> block[]
    | [p] -> block p
    | pars -> v_block (List.map block pars)
  and aux_txt_elt (t:T.text_element) : t =
    match t with
    | T.Raw txt ->
      (* remove newlines, like in LaTeX *)
      p @@ CCString.map (function '\n' -> ' ' | c->c) txt
    | T.Code txt -> pre txt
    | T.PreCode txt -> pre txt
    | T.Verbatim txt -> verbatim txt
    | T.Style (st,t) ->
      begin match st with
        | T.SK_bold -> bold (aux_txt t)
        | T.SK_italic | T.SK_emphasize | T.SK_center | T.SK_left
        | T.SK_right | T.SK_superscript | T.SK_subscript | T.SK_custom _ ->
          aux_txt t
      end
    | T.List l -> list (List.map aux_txt l)
    | T.Enum l -> enum (List.map aux_txt l)
    | T.Newline -> assert false
    | T.Title (_,_,d) ->
      (* convert title into a string *)
      section (Format.asprintf "@[<h>%a@]" pp @@ aux_txt d)
    | T.Ref (T.RK_link,d,Some txt) -> url ~url:d (to_string (aux_txt txt))
    | T.Ref (_,d,_) -> ref d
    | T.Target (_,d) -> pre d (* TODO: have a [Code _] case? *)
    | T.Special_ref _ -> s "<ocamldoc special ref>"
  and aux_tag (t:T.tag) : t =
    let tag = match t with
      | T.Author s -> OT_author s
      | T.Version s -> OT_version s
      | T.See (sr,d) -> OT_see (aux_sr sr, aux_txt d)
      | T.Since s -> OT_since s
      | T.Before (s,d) -> OT_before (s, aux_txt d)
      | T.Deprecated d -> OT_deprecated (aux_txt d)
      | T.Param (s,d) -> OT_param (s, aux_txt d)
      | T.Raised_exception (s,d) -> OT_raised_exception (s, aux_txt d)
      | T.Return_value d -> OT_return_value (aux_txt d)
      | T.Inline -> OT_inline
      | T.Custom (s,d) -> OT_custom (s, aux_txt d)
      | T.Canonical s -> OT_canonical s
    in
    mk_ (OCamldoc_tag tag)
  and aux_sr = function
    | T.See_url url -> See_url url
    | T.See_file f -> See_file f
    | T.See_doc d -> See_doc d
  in
  aux_t d

let of_string (s:string): (t, _) result =
  let open Octavius.Errors in
  let pp_loc out {line;column} = Fmt.fprintf out "%d:%d" line column in
  match Octavius.parse (Lexing.from_string s) with
  | Error {error; location={start; finish } } ->
    Error (Format.asprintf
        "@[invalid ocamldoc comment (@[%a .. %a@]):@ %a@]"
        pp_loc start pp_loc finish Fmt.text @@ Octavius.Errors.message error)
  | Ok x ->
    Ok (of_octavius x)

let of_string_exn s : t =
  match of_string s with
  | Ok x -> x
  | Error e -> failwith @@ Printf.sprintf "expected valid document,@ %s" e
