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

val map : 'a t -> f:('a -> 'b) -> 'b t

val deps : 'a t -> 'a list

