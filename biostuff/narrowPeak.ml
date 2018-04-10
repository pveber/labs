open Core_kernel

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

let parse_pValue x = match Float.of_string x with
  | -1. -> None
  | x -> Some x

let parse_peak x = match Int.of_string x with
  | -1 -> None
  | x -> Some x

let parse line =
  match (line : Biocaml_base.Line.t :> string) with
  | "" -> `Comment ""
  | line ->
    if Char.(line.[0] = '#') then
      `Comment (String.slice line 1 0)
    else if String.length line >= 6 && String.is_prefix line ~prefix:"track " then
      `Track (String.slice line 5 0)
    else (
      match String.split ~on:'\t' line with
      | [ chrom ; chromStart ; chromEnd ; name ; score ; strand ; signalValue ; pValue ; qValue ; peak ] ->
        (
          try
            `Record {
              chrom ;
              chromStart = Int.of_string chromStart ;
              chromEnd = Int.of_string chromEnd ;
              name ;
              score = Int.of_string score ;
              strand = (
                match Bed.parse_strand strand with
                | Ok s -> s
                | Error _ -> failwith ""
              ) ;
              signalValue = Float.of_string signalValue ;
              pValue = parse_pValue pValue ;
              qValue = parse_pValue qValue ;
              peak = parse_peak peak ;
            }
          with _ -> failwith line
        )
      | _ -> failwith line
    )

include Line_oriented.Make(struct
    type nonrec item = item
    let parse = parse
  end)
