
open Document
module Fmt = CCFormat

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
    let t = match t with
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
    Document.tag t
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
