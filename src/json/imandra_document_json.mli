open Imandra_document

module Encode (E : Decoders.Encode.S) : sig
  val attribute : Document.attribute E.encoder
  val attributes : Document.attributes E.encoder
  val region : Document.region E.encoder
  val t : Document.t E.encoder
end

module Decode (D : Decoders.Decode.S) : sig
  val attribute : Document.attribute D.decoder
  val attributes : Document.attributes D.decoder
  val region : Document.region D.decoder
  val t : Document.t D.decoder
end
