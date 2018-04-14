open Base

type t = {
  chr : string ;
  st : int ;
  ed : int ;
} [@@deriving compare, sexp]

let to_string { chr ; st ; ed } =
  Printf.sprintf "%s:%d-%d" chr st ed
