open Bistro

val int : int -> Template.t
val string : string -> Template.t
val list : ('a -> Template.t) -> 'a list -> Template.t
val seq : Template.t list -> Template.t
