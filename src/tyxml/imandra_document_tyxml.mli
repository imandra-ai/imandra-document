
open Imandra_document.Document

module H = Tyxml.Html

val to_doc : [`Div] Tyxml.Html.elt -> t
(** Make a document containing a snippet of html *)

val to_html_elt : t -> [> Html_types.div ] H.elt
val to_string_html_elt : t -> string

val to_html_doc :
  ?title:string ->
  ?meta:[< Html_types.meta_attrib > `Charset ] H.attrib list ->
  t -> H.doc

val to_string_html_doc :
  ?title:string ->
  ?meta:[< Html_types.meta_attrib > `Charset ] H.attrib list ->
  t -> string
(** Toplevel document *)
