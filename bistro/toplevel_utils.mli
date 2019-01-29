open Bistro

val np : int ref
val mem : int ref

val path : _ pworkflow -> string
val rm : _ pworkflow -> unit
val browse : _ pworkflow -> unit
val ls : #directory pworkflow -> unit
val less : #text_file pworkflow -> unit
val eval : 'a workflow -> 'a
