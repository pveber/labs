open Core
open Biocaml_base

module Xls = struct

  type entry = {
      seqid : string ;
      pos_start : int ;
      pos_end : int ;
      length : int ;
      summit : int ;
      tags : int ;
      pvalue : float ;
      fold : float ;
  }

  type item = [
    | `Comment of string
    | `Record of entry
    | `Header
  ]

  let header =
    "chr\tstart\tend\tlength\tsummit\ttags\t-10*log10(pvalue)\tfold_enrichment"

  let parse line =
    match (line : Line.t :> string) with
    | "" -> `Comment ""
    | line when line = header -> `Header
    | line ->
      if line.[0] = '#' then `Comment (String.slice line 1 0)
      else
        match String.split ~on:'\t' line with
        | [ seqid ; pos_start ; pos_end ; length ;
            summit ; tags ; pvalue ; fold ] ->
            `Record { seqid ;
                  pos_start = Int.of_string pos_start ;
                  pos_end = Int.of_string pos_end ;
                  length = Int.of_string length ;
                  summit = Int.of_string summit ;
                  tags = Int.of_string tags ;
                  pvalue = Float.of_string pvalue ;
                  fold = Float.of_string fold ; }
    | _ -> assert false

  let unparse = function
    | `Comment "" -> ""
    | `Comment c -> sprintf "#%s\n" c
    | `Header -> header ^ "\n"
    | `Record r ->
      sprintf "%s\n"
        (String.concat ~sep:"\t" [
            r.seqid ; sprintf "%d" r.pos_start ; sprintf "%d" r.pos_end ;
            sprintf "%d" r.length ; sprintf "%d" r.summit ;
            sprintf "%d" r.tags ; sprintf "%g" r.pvalue ;
            sprintf "%g" r.fold ;
          ]
        )
    | _ -> assert false

  (* let summits_to_bed5 = function
   *   | `Comment c -> `Comment c
   *   | `Header -> `Comment ""
   *   | `Record r ->
   *     `Record { chrom = r.seqid ;
   *               chromStart = r.pos_start + r.summit - 1 ;
   *               chromEnd = r.pos_start + r.summit ;
   *               Bed.Bed5.name =
   *                 r.seqid ^ "." ^ Int.to_string (r.pos_start + r.summit - 1)
   *                 ^ "." ^ Int.to_string (r.pos_start + r.summit) ;
   *               score = r.pvalue } *)

  (* let to_bed5 = function
   *   | `Comment c -> `Comment c
   *   | `Header -> `Comment ""
   *   | `Record r ->
   *     `Record { chrom = r.seqid ; chromStart = r.pos_start ;
   *               chromEnd = r.pos_end ;
   *               Bed.Bed5.name = r.seqid ^ "." ^ Int.to_string r.pos_start
   *                               ^ "." ^ Int.to_string r.pos_end ;
   *               score = r.pvalue } *)

end
