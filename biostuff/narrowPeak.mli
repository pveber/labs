open Biocaml_base

type t = {
  chrom : string ;
  chromStart : int ;
  chromEnd : int ;
  name : string ;
  score : int ;
  strand : [ `Plus | `Minus | `Not_relevant | `Unknown ] ;
  signalValue : float ;
  pValue : float option ; (** -log10 pval *)
  qValue : float option ; (** -log10 qval *)
  peak : int option ;
}

type item = [
  | `Comment of string
  | `Record of t
  | `Track of string
]

val parse : Line.t -> item
