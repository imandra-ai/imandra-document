
(** {1 Parsing} *)

(** We use {{: https://github.com/ocaml-doc/octavius/} Octavius} to
    parse OCamldoc markup into a document. *)
val of_string : string -> (Document.t, string) result

val of_string_exn : string -> Document.t
(** Unsafe alias to {!of_string}
    @raise Failure if the string does not parse *)

