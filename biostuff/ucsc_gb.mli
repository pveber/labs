module Chrom_size : sig
  type t = {
    chrom : string ;
    size : int ;
  }
  [@@deriving fields, csv]

  val load : string -> t list
end
