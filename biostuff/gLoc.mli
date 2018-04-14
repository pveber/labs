type t = {
  chr : string ;
  st : int ;
  ed : int ;
} [@@deriving compare, sexp]

val to_string : t -> string
