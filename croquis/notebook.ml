open Core_kernel

type element =
  | H2 of string
  | Text of string
  | Svg of string

type t = element list

let empty = []
let ( >> ) doc elt = elt :: doc

let text s = Text s

let svg s = Svg s

let section s = H2 s

let read_picture fn =
  In_channel.read_all fn
  |> Base64.encode_exn

let saveplot format f =
  let tmp = Caml.Filename.temp_file "temp" ".svg" in
  let contents =
    protect
      ~f:(fun () ->
          let _ = f tmp in
          read_picture tmp)
      ~finally:(fun () -> Sys.remove tmp)
  in
  let format = match format with
    | `svg -> "svg+xml"
    | `png -> "png"
  in
  Svg (sprintf "data:image/%s;base64,%s" format contents)


(* let vgplot f = *)
(*   let open Gg in *)
(*   let open Vg in *)
(*   saveplot `svg (fun tmp -> *)
(*       let size = Size2.v 100. 100. in *)
(*       let view = Box2.v P2.o (Size2.v 1. 1.) in *)
(*       let image = f () in *)
(*       let xmp = Vgr.xmp () in *)
(*       Out_channel.with_file tmp ~f:(fun oc -> *)
(*           let r = Vgr.create (Vgr_svg.target ~xmp ()) (`Channel oc) in *)
(*           ignore (Vgr.render r (`Image (size, view, image))); *)
(*           ignore (Vgr.render r `End) *)
(*         ) *)
(*     ) *)

let vgplot_png f =
  let open Gg in
  let open Vg in
  saveplot `png (fun tmp ->
      let res = 200. /. 0.0254 (* 300dpi in dots per meters *) in
      let fmt = `Png (Size2.v res res) in
      let size = Size2.v 100. 100. in
      let view = Box2.v P2.o (Size2.v 1. 1.) in
      let image = f () in
      Out_channel.with_file tmp ~f:(fun oc ->
          let r = Vgr.create (Vgr_cairo.stored_target fmt) (`Channel oc) in
          ignore (Vgr.render r (`Image (size, view, image)));
          ignore (Vgr.render r `End)
        )
    )


let heatmap mat =
  let open Gg in
  let open Vg in
  let n = float @@ Array.length mat in
  vgplot_png (fun () ->
      Array.foldi mat ~init:I.void ~f:(fun i acc mat_i ->
          Array.foldi mat_i ~init:acc ~f:(fun j acc mat_i_j ->
              let r =
                Box2.v
                  (V2.v (float j /. n) ((n -. float i) /. n))
                  (V2.v (1. /. n) (1. /. n))
              in
              I.blend
                (I.cut (P.empty |> P.rect r) (I.const (if mat_i_j > 0. then Color.v mat_i_j 0. 0. 1. else Color.v 0. 0. (-. mat_i_j) 1.)))
                acc
            )
        )
    )


let render elts =
  let open Tyxml_html in
  let contents =
    List.rev_map elts ~f:(function
        | Text s -> p [ txt s ]
        | Svg svg ->
          img ~src:svg ~alt:"" ()
        | H2 s -> h2 [ txt s ]
      )
  in
  html (head (title (txt "evolnet")) []) (body contents)

let to_file fn doc =
  Out_channel.with_file fn ~f:(fun oc ->
      Tyxml_html.pp () (Format.formatter_of_out_channel oc) (render doc)
    )
