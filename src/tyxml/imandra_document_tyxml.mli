
open Imandra_document.Document

module H = Tyxml.Html

module Mapper : sig
  type doc = t
  type t = {
    to_doc: t -> depth:int -> doc -> Html_types.div_content_fun H.elt;
    attr_header: t -> col:int -> string -> Html_types.th_attrib H.attrib list;
    attr_row: t -> row:int -> col:int -> doc -> Html_types.td_attrib H.attrib list;
  }
  (** Translate a document to html *)

  val default : t
  (** Default mapper, does deep recursion *)

  val run_elt : t -> doc -> [> Html_types.div ] H.elt
  (** Get an element *)

  val run_doc :
    ?title:string ->
    ?meta:[< Html_types.meta_attrib > `Charset ] H.attrib list ->
    ?headers:Html_types.head_content_fun H.elt list ->
    t -> doc -> H.doc
  (** Get a full HTML document *)
end

val to_doc : [`Div] Tyxml.Html.elt -> t
(** Make a document containing a snippet of html *)

val to_html_elt : t -> [> Html_types.div ] H.elt
val to_string_html_elt : t -> string

val string_of_html_elt : [< Html_types.div ] H.elt -> string
val string_of_html_doc : H.doc -> string

val to_html_doc :
  ?title:string ->
  ?meta:[< Html_types.meta_attrib > `Charset ] H.attrib list ->
  ?headers:Html_types.head_content_fun H.elt list ->
  t -> H.doc

val to_string_html_doc :
  ?title:string ->
  ?meta:[< Html_types.meta_attrib > `Charset ] H.attrib list ->
  ?headers:Html_types.head_content_fun H.elt list ->
  t -> string
(** Toplevel document *)
