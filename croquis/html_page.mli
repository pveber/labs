type t = Tyxml_html.doc

val make :
  title:string ->
  [< Html_types.body_content_fun ] Tyxml_html.elt list ->
  t

val to_file : t -> string -> unit
