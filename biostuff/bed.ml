open Core
open Biocaml_base
open Printf

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

let parse_strand = function
  | "." -> Ok `Not_relevant
  | "?" -> Ok `Unknown
  | "+" -> Ok `Plus
  | "-" -> Ok `Minus
  | s -> Error s

let parse_item f line =
  match (line : Line.t :> string) with
  | "" -> `Comment ""
  | line ->
    if Char.(line.[0] = '#')
    then `Comment (String.slice line 1 0)
    else
      let fields = String.split ~on:'\t' line in
      `Record (f fields)

let unparse_item f = function
  | `Comment c -> sprintf "#%s\n" c
  | `Record r ->
      sprintf "%s\n"
        (String.concat ~sep:"\t" (f r))

(* module type Variant = sig
 *   type t
 *   val parse : Line.t -> t item
 *   val reader : (Line.t, t item, unit) Pipe.t
 *   val unparse : t item -> string
 *   val writer : (t item, string, unit) Pipe.t
 * end *)

module Bed3 = struct
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
  }

  let from_fields = function
    | [ chrom ; chromStart ; chromEnd ] ->
      { chrom ;
        chromStart = Int.of_string chromStart ;
        chromEnd = Int.of_string chromEnd ;
      }
    | _ -> assert false

  let to_fields r = [
    r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd
  ]

  let parse = parse_item from_fields
  let unparse = unparse_item to_fields
end

module Bed4 = struct
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
  }

  let from_fields = function
    | [ chrom ; chromStart ; chromEnd ; name ] ->
      { chrom ;
        chromStart = Int.of_string chromStart ;
        chromEnd = Int.of_string chromEnd ;
        name }
    | _ -> assert false

  let to_fields r = [
    r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ; r.name
  ]


  let parse = parse_item from_fields
  let unparse = unparse_item to_fields
end

module Bed5 = struct
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : float ;
  }

  let from_fields = function
    | [ chrom ; chromStart ; chromEnd ; name ; score ] ->
      { chrom ;
        chromStart = Int.of_string chromStart ;
        chromEnd = Int.of_string chromEnd ;
        name ; score = Float.of_string score }
    | _ -> assert false

  let to_fields r = [
    r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ;
    r.name ; sprintf "%g" r.score
  ]

  let to_bed4 = function
    | `Comment c -> `Comment c
    | `Record r ->
      `Record { Bed4.chrom = r.chrom ; chromStart = r.chromStart ;
                chromEnd = r.chromEnd ; name = r.name }


  let parse = parse_item from_fields
  let unparse = unparse_item to_fields

end

module Bed6 = struct
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : float ;
    strand : strand ;
  }

  let from_fields = function
    | [ chrom ; chromStart ; chromEnd ; name ; score ; strand ] ->
      { chrom ;
        chromStart = Int.of_string chromStart ;
        chromEnd = Int.of_string chromEnd ;
        name ;
        score = Float.of_string score ;
        strand = (
          match parse_strand strand with
          | Ok s -> s
          | Error msg -> failwith msg
        ) ;
      }
    | _ -> assert false

  let to_fields r = [
    r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ;
    r.name ; sprintf "%g" r.score ;
    (match r.strand with
     | `Not_relevant -> "."
     | `Unknown -> "?"
     | `Plus -> "+"
     | `Minus -> "-" )
  ]

  let parse = parse_item from_fields
  let unparse = unparse_item to_fields

end
