open Biocaml_base

type 'a item = [
  | `Comment of string
  | `Record of 'a
]

type strand = [
  | `Plus
  | `Minus
  | `Not_relevant
  | `Unknown
]
val parse_strand : string -> (strand, string) result

module Bed3 : sig
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
  }
  val parse : Line.t -> t item
  val unparse : t item -> string
end

module Bed4 : sig
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
  }
  val parse : Line.t -> t item
  val unparse : t item -> string
end

module Bed5 : sig
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : float ;
  }
  val parse : Line.t -> t item
  val unparse : t item -> string
  val to_bed4 : t item -> Bed4.t item
end

module Bed6 : sig
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : float ;
    strand : strand ;
  }
end
