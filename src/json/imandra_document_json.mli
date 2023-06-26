open Imandra_document

module Encode (E : Decoders.Encode.S) : sig
  val attribute : Document.attribute E.encoder
  val attributes : Document.attributes E.encoder
  val region : Document.region E.encoder
  val view : Document.view E.encoder
  val t : Document.t E.encoder
end
