open Core

type 'a token =
  | S of string
  | D of 'a
  | F of 'a t (* Fragment *)
  | DEST
  | TMP
  | NP
  | MEM
  | EXE

and 'a t = 'a token list

let rec deps tmpl =
  List.map tmpl ~f:(function
      | D r -> [ r ]
      | F toks -> deps toks
      | S _ | DEST | TMP | NP | MEM | EXE -> []
    )
  |> List.concat
  |> List.dedup_and_sort ~compare:Pervasives.compare


let rec map_token x ~f = match x with
  | S s -> S s
  | D dep -> D (f dep)
  | F toks -> F (List.map toks ~f:(map_token ~f))
  | DEST -> DEST
  | TMP -> TMP
  | NP -> NP
  | MEM -> MEM
  | EXE -> EXE

let map toks ~f = List.map toks ~f:(map_token ~f)
