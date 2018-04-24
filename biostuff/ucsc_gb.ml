module Chrom_size = struct
  type t = {
    chrom : string ;
    size : int ;
  }
  [@@deriving fields, csv]

  let load fn = csv_load ~separator:'\t' fn
end
