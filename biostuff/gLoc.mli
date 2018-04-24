type t = {
  chr : string ;
  st : int ;
  ed : int ;
} [@@deriving compare, sexp]

val of_string : string -> (t, [> `Parse_error]) result
val of_string_exn : string -> t
val to_string : t -> string
