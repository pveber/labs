open Core
open Biocaml_base

module Record = struct
  type t = Gff.record = {
    seqname    : string ;
    source     : string option ;
    feature    : string option ;
    start_pos  : int ;
    stop_pos   : int ;
    score      : float option ;
    strand     : [`Plus | `Minus | `Not_stranded | `Unknown ] ;
    phase      : int option ;
    attributes : (string * string list) list ;
  }
  [@@deriving sexp]

  let loc r = GLoc.{ chr = r.seqname ; st = r.start_pos ; ed = r.stop_pos }
end

module Item = struct
  type t = Gff.item

  let parse line =
    match Gff.gff3_item_of_line line with
    | Ok item -> item
    | Error (`Msg msg) -> failwith msg

  let to_record = function
    | `Record r -> Some r
    | `Comment _ -> None
end

include Line_oriented.Make(Item)
