open Biocaml_base

type 'a item = [
  | `Comment of string
  | `Record of 'a
]

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
    strand : [ `Plus | `Minus | `Not_relevant | `Unknown ] ;
  }
end
