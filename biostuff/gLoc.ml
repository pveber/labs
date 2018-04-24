open Base

type t = {
  chr : string ;
  st : int ;
  ed : int ;
} [@@deriving compare, sexp]

let to_string { chr ; st ; ed } =
  Printf.sprintf "%s:%d-%d" chr st ed

let of_string_exn s =
  try Caml.Scanf.sscanf s "%[^:]:%d-%d" (fun chr st ed -> { chr ; st ; ed })
  with _ -> failwith ("GLoc.of_string_exn: " ^ s)

let%test "of_string_exn_1" =
  Caml.(
    of_string_exn "chr1:3053032-3053034"
    =
    { chr = "chr1" ; st = 3053032 ; ed = 3053034 }
  )

let of_string s =
  try Ok (of_string_exn s)
  with _ -> Error `Parse_error
