(** {1 Simple Formatted Document} *)

(** A {!Document.t} value represents a structured document. It can be
    rendered in several ways, depending on the frontend (pure text, HTML, …).

    In Imandra, a document printer for type [foo] (of type [foo -> Document.t])
    can be installed using:

{[
  #install_doc doc_of_foo;;
]}

    A good example is in the
    {{: https://docs.imandra.ai/imandra-docs/notebooks/sudoku/} Sudoku solving with Imandra notebook}
*)

(* This file is free software. See file "license" for more details. *)

(** Attributes for modifying the style of a document
    or influencing rendering in the browser or terminal *)
type attribute =
  | A_color of string (** ANSI color *)
  | A_class of string (** DOM class *)
  | A_custom of string * string

type attributes = attribute list

type t
(** A structured document that can be displayed in nice ways in
    various frontends (terminal, browser, etc.)

    Documents are built using a variety of functions that can be found below. *)

type doc = t

type region =
  { constraints : string list;
    invariant : string }
(** A region obtained from the decomposition of some function.
    See the [Modular_decomp] module in Imandra for more details. *)

(** View of the root of a document.

    This view can be obtained using {!view} and is used to introspect
    the root of a document. It should be mostly useful for implementing
    a new renderer. *)
type view =
  | Section of string (** section (title) *)
  | String of string (** raw string *)
  | Text of string (** text with flexible newlines and spaces *)
  | Pre of string (** pre-formatted paragraph *)
  | Block of t list (** A set of sub-documents, with no particular formatting *)
  | V_block of t list (** vertical list of docs, separated by empty lines *)
  | Indented of string * t (** indented block with header *)
  | List of {
      bullet: string; (** bullet for each list item *)
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
    } (** URL with an alt-text *)
  | OCamldoc_ref of string (** OCamldoc-specific reference *)
  | OCamldoc_tag of ocamldoc_tag (** OCamldoc-specific tag *)
  | Fold of {
      folded_by_default: bool; (** is it originally folded? *)
      summary: string; (** short summary to print when it's folded *)
      sub: t; (** full document to print when it's unfolded *)
    }
  (** A foldable document, with a short summary. In an interactive renderer
      such as a browser, this should be rendered as a foldable item that can be
      opened and closed by the user. *)
  | Alternatives of {
      views: (string * t) list; (** possible ways of representing the same thing *)
    } (** Alternative views on something. These views are mutually exclusive
          and each one has its own label. *)
  | Regions of region list (** Special view for decomposition (see {!region}) *)
  | Html of [`Div] Tyxml.Html.elt

(** OCamldoc-specific "see" reference. *)
and ocamldoc_see_ref =
  | See_url of string
  | See_file of string
  | See_doc of string

(** OCamldoc-specific tags *)
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

val view : t -> view
(** View the root of the given document *)

val attrs : t -> attributes
(** Attributes attached to the root of the given document *)

(* TODO: recursive partition, to be converted into  Voronoi regions *)

(** {2 Builders} *)

(** Convenience builders for producing documents *)

val empty : t (** Empty document, doesn't display anything. *)

val block : ?a:attributes -> t list -> t (** Block composed of sub-documents *)

val block_of : ?a:attributes -> ('a -> t) -> 'a list -> t (** List.map + {!block} *)

val v_block : ?a:attributes -> t list -> t (** Vertical block *)

val v_block_of : ?a:attributes -> ('a -> t) -> 'a list -> t (** List.map + {!v_block} *)

val section : ?a:attributes -> string -> t (** A title section *)

val section_f : ?a:attributes -> ('a, Format.formatter, unit, t) format4 -> 'a
(** {!Format}-aware version of {!section} *)

val s : string -> t (** basic string *)

val s_f : ('a, Format.formatter, unit, t) format4 -> 'a
(** {!Format}-aware version of {!s} *)

val int : int -> t (** Print an integer *)

val bigint : Z.t -> t (** Print an arbitrary precision integer *)

val rat : Q.t -> t  (** Print an arbitrary precision rational *)

val p : ?a:attributes -> string -> t (** simple paragraph *)

val p_f : ?a:attributes -> ('a, Format.formatter, unit, t) format4 -> 'a
(** {!Format}-aware version of {!p} *)

val paragraph : ?a:attributes -> string -> t (** Alias for {!p} *)

val paragraph_f : ?a:attributes -> ('a, Format.formatter, unit, t) format4 -> 'a
(** Alias for {!p_f} *)

val pre : ?a:attributes -> string -> t
(** Pre-rendered text *)

val pre_f : ?a:attributes -> ('a, Format.formatter, unit, t) format4 -> 'a
(** {!Format}-aware version of {!pre} *)

val list : ?a:attributes -> ?bullet:string -> t list -> t
  (** List of documents with optional bullet points *)

val list1 : ?a:attributes -> ?bullet:string -> t list -> t
(** Same as {!list} but turns [[d]] into [d] (ie. reduces a list with
    one element into the element itself) *)

val list_of : ?a:attributes -> ('a -> t) -> 'a list -> t
(** List.map + {!list} *)

val list_of1 : ?a:attributes -> ('a -> t) -> 'a list -> t
(** List.map + {!list1} *)

val enum : ?a:attributes -> t list -> t
(** Enumerated list *)

val indent : ?a:attributes -> string -> t -> t
(** Indented block (same as "description" in LaTeX) *)

val tbl : ?a:attributes -> ?headers:string list -> t list list -> t
(** Table, with optional headers. Each element of the list is a row of
    the table, and all rows must have the same length *)

val tbl_of : ?a:attributes -> ?headers:string list -> ('a -> t) -> 'a list list -> t
(** Use the function to build a table of documents. *)

val tbl_of_rows : ?a:attributes -> ?headers:string list -> ('row -> t list) -> 'row list -> t
(** Combination of List.map + {!tbl} *)

val graphviz : ?a:attributes -> string -> t
(** Graphviz block. Use as follow:

    {[
      Document.graphviz {|
        digraph foo {
          1 -> 2;
          1 -> 3;
          2 -> 3;
        }
    |}
    ]}
*)

val bold : ?a:attributes -> t -> t
(** Use bold style on the document *)

val italic : ?a:attributes -> t -> t
(** Use italic style on the document *)

val verbatim : ?a:attributes -> string -> t

val tag : ?a:attributes -> ocamldoc_tag -> t
(** Add an OCamldoc tag *)

val ref : ?a:attributes -> string -> t
(** Add an OCamldoc reference *)

val url : ?a:attributes -> url:string -> string -> t
(** Url with description *)

val fold : ?a:attributes -> ?folded_by_default:bool -> ?summary:string -> t -> t
(** Make a foldable document.

    @param folded_by_default if true, the document should be folded by default,
    otherwise it will be unfolded by default
    @param summary a short string to display in place of the document
    when it is folded *)

val alternatives : ?a:attributes -> (string * t) list -> t
(** A list of alternative views on the same thing. Only one alternative
    should be displayed at a time (e.g. using tabs).

    If the list contains only one element, the element is returned instead.
    If the list is empty, the empty document is returned instead.
*)

val regions : region list -> t
(** Decomposition regions *)

val html : [`Div] Tyxml.Html.elt -> t
(** html string for UI display *)

val record : ?a:attributes -> (string * t) list -> t
(** A list of key/value entries *)

val intersperse : t -> t list -> t list
(** [intersperse x l] puts [x] between each element of [l] *)

val some : t option -> t
(** Extract the option, or return {!empty} *)

val map_opt : ('a -> t) -> 'a option -> t
(** Transform [Some x] into a document using the function on [x],
    or return {!empty} *)

(** {2 Attribute builders} *)
module A : sig
  type t = attribute

  val color : string -> t

  val red : t
  val blue : t
  val green : t
  val yellow : t

  val cls: string -> t
end

(** {2 Graph Builder}

    A convenient module for building simple graphs and turning them into
    Graphviz code.

    - Use [Graph.e ?lbl source_name destination_name] to describe an edge
      of the graph (with an optional label).
    - Use [Graph.n ?lbl node_name] to describe a node of the graph
      (with an optional label). Note that nodes that appear in at least one
      edge are not required to be specified with [n].
    - Use [Graph.set_graph_name s] (at most once) to give a name to the
      graph.

    Example:
    {[
      type g = { edges: (int * int) list; }

      #program;;
      let g_to_doc g =
        let module D = Document in
        D.Graph.(to_doc @@ List.map (fun (a,b) -> e (Int.to_string a) (Int.to_string b)) g.edges)
      ;;

      #install_doc g_to_doc;;

      {edges=[1,2; 3,4; 1,4; 2,10; 10,1; 3,2; 4,10] };;
    ]}
*)
module Graph : sig
  type event = private
    | Set_graph_name of string
    | Node of {name: string; label: string}
    | Edge of {src: string; target: string; label: string}

  type t = event list

  val set_graph_name : string -> event
  val n : ?lbl:string -> string -> event
  val e : ?lbl:string -> string -> string -> event

  val to_doc : ?a:attributes -> t -> doc
end

(** {2 Rendering} *)

val pp : Format.formatter -> t -> unit
(** Regular pretty printer *)

val pp_ocamldoc_tag : Format.formatter -> ocamldoc_tag -> unit
val pp_ocamldoc_see_ref : Format.formatter -> ocamldoc_see_ref -> unit

val to_string : t -> string

module H = Tyxml.Html

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
