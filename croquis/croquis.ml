open Core_kernel
open Gg
open Vg

module Viewport = struct
  type t = {
    scale_x : float -> float ;
    scale_y : float -> float ;
  }

  let linear ~xlim:(x_lo, x_hi) ~ylim:(y_lo, y_hi) ~size:(w, h) =
    let xrho = w /. (x_hi -. x_lo) in
    let yrho = h /. (y_hi -. y_lo) in
    let scale_x x = (x -. x_lo) *. xrho in
    let scale_y y = (y -. y_lo) *. yrho in
    { scale_x ; scale_y }

  let id = {
    scale_x = ident ;
    scale_y = ident ;
  }
  let scale_x vp = vp.scale_x
  let scale_y vp = vp.scale_y

  let scale vp pt =
    V2.(v (vp.scale_x (x pt)) (vp.scale_y (y pt)))
end

class type t = object
  method image : image
  method bbox : Box2.t
end

type arrow_style = [
  | `none
  | `triangle
]

(* let arrow_tip p = function
 *   | `none -> p
 *   | `triangle *)

class path ?(vp = Viewport.id) ?(col = Color.black) ?(tip = `none) origin points =
  object (s)
    method image =
      let path =
        List.fold points
          ~init:(P.empty >> P.sub (Viewport.scale vp origin))
          ~f:(fun acc p -> acc >> P.line (Viewport.scale vp p))
      in
      I.cut path (I.const col)
    method bbox = Box2.of_pts s#start s#_end_
    method start = origin
    method _end_ =
      match points with
      | [] -> s#start
      | _ -> List.last_exn points (* [points] is not empty *)
  end

let render croquis fn =
  let c = 10. in
  let view = croquis#bbox in
  let bbox_size = Box2.size view in
  let w = V2.x bbox_size and h = V2.y bbox_size in
  let size =
    if w > h then V2.v c (h *. c /. w)
    else V2.v (w *. c /. h) h
  in
  let image = croquis#image in
  Out_channel.with_file fn ~f:(fun oc ->
      let r = Vgr.create (Vgr_pdf.target ()) (`Channel oc) in
      ignore (Vgr.render r (`Image (size, view, image))) ;
      ignore (Vgr.render r `End)
    )

(* let genome () = *)
(*   let loc_start = 330000. and loc_end = 890000. in *)
(*   let nb_tracks = 2 in *)

(*   let margin = 0.05 in *)

(*   let vp = Viewport.make ~xlim:(loc_start, loc_end) ~ylim:(0., float nb_tracks) in *)
(*   new line *)

let demo () =
  (* let croquis = genome () in *)
  let croquis = new path V2.zero [] in
  render croquis "rien.pdf"
