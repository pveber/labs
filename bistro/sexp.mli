open Bistro

val int : int -> template
val string : string -> template
val list : ('a -> template) -> 'a list -> template
val seq : template list -> template
