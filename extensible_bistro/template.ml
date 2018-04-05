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
  |> List.dedup_and_sort


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

module EDSL = struct
  let dest = [ DEST ]
  let tmp = [ TMP ]
  let np = [ NP ]
  let mem = [ MEM ]
  let exe = [ EXE ]

  let string s = [ S s ]
  let int i = string (string_of_int i)
  let float f = string (Float.to_string f)
  let dep w = [ D w ]

  let quote ?using:(c = '"') e =
    let quote_symbol = S (Char.to_string c) in
    quote_symbol :: e @ [ quote_symbol ]

  let option f = function
    | None -> []
    | Some x -> f x

  let list f ?(sep = ",") l =
    List.map l ~f
    |> List.intersperse ~sep:(string sep)
    |> List.concat

  let seq ?sep xs =
    let format = match sep with
      | None -> ident
      | Some sep -> List.intersperse ~sep:(string sep)
    in
    List.concat (format xs)

  let enum dic x = string (List.Assoc.find_exn ~equal:( = ) dic x)

  let file_dump contents = [ F contents ] (* FIXME: should check that there is no file_dump in contents *)
end

include EDSL
