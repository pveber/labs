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

type thickness = [
  | `normal
  | `thick
]

let thickness_value = function
  | `normal -> 0.01
  | `thick -> 0.02

(* let arrow_tip p = function
 *   | `none -> p
 *   | `triangle *)

let circle ?(col = Color.black) ?(thickness = `normal) ~center ~radius () =
  object
    method image =
      let area = `O { P.o with P.width = thickness_value thickness } in
      I.cut ~area (P.empty |> P.circle center radius) (I.const col)
    method bbox =
      let r = 2. *. radius +. thickness_value thickness in
      Box2.v_mid center (V2.v r r)
  end

let rectangle ?(vp = Viewport.id) ?(col = Color.black) ?(thickness = `normal) ~center ~size () =
  let dx = V2.x size /. 2. in
  let dy = V2.y size /. 2. in
  let sw = V2.add center (V2.v (-. dx) (-. dy)) in
  let nw = V2.add center (V2.v (-. dx) (   dy)) in
  let ne = V2.add center (V2.v (   dx) (   dy)) in
  let se = V2.add center (V2.v (   dx) (-. dy)) in
  object
    method image =
      let area = `O { P.o with P.width = thickness_value thickness } in
      let p =
        P.empty
        |> P.sub (Viewport.scale vp sw)
        |> P.line (Viewport.scale vp nw)
        |> P.line (Viewport.scale vp ne)
        |> P.line (Viewport.scale vp se)
        |> P.line (Viewport.scale vp sw)
      in
      I.cut ~area p (I.const col)
    method bbox = Box2.of_pts sw ne
  end

let rectangle' ?vp ?col ?thickness ~xmin ~xmax ~ymin ~ymax () =
  let center = V2.v ((xmax +. xmin) /. 2.) ((ymax +. ymin) /. 2.) in
  let size = V2.v (xmax -. xmin) (ymax -. ymin) in
  rectangle ?vp ?col ?thickness ~center ~size ()

let stack xs =
  object
    method image =
      List.fold xs ~init:I.void ~f:(fun acc x -> I.blend acc x#image)
    method bbox =
      List.fold xs ~init:Box2.empty ~f:(fun acc x -> Box2.union acc x#bbox)
  end

let text ?(pos = V2.zero) txt =
  let font_size = 0.3 in
  let font = {
    Font.name = "Helvetica" ;
    slant = `Normal ;
    weight = `W400 ;
    size = font_size ;
  }
  in
  let w = Font_utils.text_width font_size txt in
  object
    method image =
      let glyphs =
        String.fold txt ~init:[] ~f:(fun acc c -> Char.to_int c :: acc)
        |> List.rev
      in
      I.cut_glyphs font glyphs (I.const Color.black)
      |> I.move (V2.v (-. w /. 2.) 0.)
      |> I.move pos
    method bbox = Box2.v_mid pos (V2.v w 1.) (* FIXME *)
  end

let path ?(vp = Viewport.id) ?(col = Color.black) ?tip:(_tip = `none) origin points =
  object (s)
    method image =
      let path =
        List.fold points
          ~init:(P.empty |> P.sub (Viewport.scale vp origin))
          ~f:(fun acc p -> acc |> P.line (Viewport.scale vp p))
      in
      I.cut path (I.const col)
    method bbox = Box2.of_pts s#start s#_end_
    method start = origin
    method _end_ =
      match points with
      | [] -> s#start
      | _ -> List.last_exn points (* [points] is not empty *)
  end

let intersection2 (type s) ~compare ~a ~b =
  let module E = struct type t = s let compare = compare end in
  let module S = Caml.Set.Make(E) in
  let set_a = S.of_list a in
  let set_b = S.of_list b in
  S.cardinal (S.inter set_a set_b),
  S.cardinal (S.diff set_a set_b),
  S.cardinal (S.diff set_b set_a)

let venn_diagram2 ~compare ~a ~b () =
  let n_ab, n_a, n_b = intersection2 ~compare ~a ~b in
  let delta = 1. in
  let r = 1.8 in
  let center_a = V2.v (-. delta) 0. in
  let center_b = V2.v     delta  0. in
  stack [
    circle ~center:center_a ~radius:r () ;
    circle ~center:center_b ~radius:r () ;
    text (Int.to_string n_ab) ;
    text ~pos:(V2.v (-. r) 0.) (Int.to_string n_a) ;
    text ~pos:(V2.v r 0.)      (Int.to_string n_b) ;
  ]

let venn_diagram3 () =
  let delta = 1. in
  let radius = 1.8 in
  let center_a = V2.v 0. delta in
  let center_b = V2.v (delta *. sqrt 3. /. 2.) (-. delta /. 2.) in
  let center_c = V2.v (-. delta *. sqrt 3. /. 2.) (-. delta /. 2.) in
  stack [
    circle ~center:center_a ~radius () ;
    circle ~center:center_b ~radius () ;
    circle ~center:center_c ~radius () ;
  ]

let render croquis fn =
  let c = 100. in
  let view = croquis#bbox in
  let bbox_size = Box2.size view in
  let w = V2.x bbox_size and h = V2.y bbox_size in
  let size =
    if w > h then V2.v c (h *. c /. w)
    else V2.v (w *. c /. h) c
  in
  let font _ = `Helvetica in
  let image = croquis#image in
  Out_channel.with_file fn ~f:(fun oc ->
      let r = Vgr.create (Vgr_pdf.target ~font ()) (`Channel oc) in
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
  (* let croquis = venn_diagram2 ~compare:Int.compare ~a:[1;2] ~b:[1] () in *)
  let croquis = venn_diagram3 () in
  render croquis "rien.pdf"
