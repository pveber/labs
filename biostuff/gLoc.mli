type t = {
  chr : string ;
  st : int ;
  ed : int ;
} [@@deriving compare]

val to_string : t -> string
