type t
type element

val empty : t
val ( >> ) : t -> element -> t

val text : string -> element
val svg : string -> element
val heatmap :
  float array array -> element

val section : string -> element

val render : t -> [> `Html ] Tyxml_html.elt

val to_file : string -> t -> unit
