open Core_kernel

type t = [ `Html ] Tyxml_html.elt

let bootstrap_head t =
  let open Tyxml_html in
  head (title (txt t)) [
    link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css" () ;
    link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css" () ;
    script ~a:[a_src "https://code.jquery.com/jquery.js"] (txt "") ;
    script ~a:[a_src "http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js"] (txt "") ;
  ]

let make ~title:t contents =
  let open Tyxml_html in
  html (bootstrap_head t) (body [ div ~a:[a_class ["container"]] contents ])

let to_file doc fn =
  Out_channel.with_file fn ~f:(fun oc ->
      Tyxml_html.pp () (Format.formatter_of_out_channel oc) doc
    )
