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

let strictly_before x y =
  match String.compare x.chr y.chr with
  | -1 -> true
  |  1 -> false
  |  0 -> x.ed < y.st
  | _ -> assert false

let%test "strictly_before_1" =
  strictly_before { chr = "a" ; st = 0 ; ed = 4 } { chr = "b" ; st = 0 ; ed = 4 }

let%test "strictly_before_2" =
  strictly_before { chr = "a" ; st = 0 ; ed = 4 } { chr = "a" ; st = 10 ; ed = 40 }

let%test "strictly_before_3" =
  not (
    strictly_before { chr = "a" ; st = 0 ; ed = 4 } { chr = "a" ; st = 4 ; ed = 40 }
  )

let intersects x y =
  String.(x.chr = y.chr)
  && (
    (x.st <= y.st && y.st <= x.ed)
    || (y.st <= x.st && x.st <= y.ed)
  )

let%test "intersects_1" =
  intersects { chr = "a" ; st = 0 ; ed = 4 } { chr = "a" ; st = 2 ; ed = 30 }
