(* taken from vecho.ml in vg *)
open Core_kernel
open Gg
open Vg

module Gmap = Map.Make(Int) (* glyph maps *)
module Cmap = Gmap (* uchar maps *)

let str = Printf.sprintf
let otfm_err_str err =
  Format.fprintf Format.str_formatter "%a" Otfm.pp_error err;
  Format.flush_str_formatter ()


type otf_info =
  { i_otf : string;                                      (* The font bytes. *)
    i_cmap : int Cmap.t;           (* Maps unicode scalar values to glyphs. *)
    i_advs : int Gmap.t;             (* Maps glyph to advances in em space. *)
    i_kern : int Gmap.t Gmap.t;    (* Maps glyph pairs to kern adjustement. *)
    i_units_per_em : int; }

let add_adv acc g adv _ = Gmap.set acc g adv

let add_cmap acc kind (u0, u1) g =
  let acc = ref acc in
  begin match kind with
  | `Glyph_range ->
      for i = 0 to (u1 - u0) do acc := Cmap.set !acc (u0 + i) (g + i) done;
  | `Glyph ->
      for u = u0 to u1 do acc := Cmap.set !acc u g done
  end;
  !acc

let add_ktable acc i =
  (if i.Otfm.kern_dir = `H && i.Otfm.kern_kind = `Kern then `Fold else `Skip),
  acc

let add_kpair acc g0 g1 kv =
  let m = try Gmap.find_exn acc  g0 with Caml.Not_found -> Gmap.empty in
  Gmap.set acc g0 (Gmap.set m g1 kv )

let font_info inf = 
  let i_otf = In_channel.read_all inf in
  let ( >>= ) x f = match x with
    | Error e -> Error (str "%s: %s" inf (otfm_err_str e))
    | Ok v -> f v
  in
  let d = Otfm.decoder (`String i_otf) in
  Otfm.postscript_name d                      >>= fun name ->
  Otfm.head d                                 >>= fun head ->
  Otfm.cmap d add_cmap Cmap.empty             >>= fun (_, i_cmap) ->
  Otfm.hmtx d add_adv Gmap.empty              >>= fun i_advs ->
  Otfm.kern d add_ktable add_kpair Gmap.empty >>= fun i_kern ->
  let name = match name with None -> "Unknown" | Some n -> n in
  let i_units_per_em = head.Otfm.head_units_per_em in
  Ok (name, { i_otf; i_cmap; i_advs; i_kern; i_units_per_em })

let get_glyph fi g =
  Gmap.find fi.i_cmap g
  |> Option.value ~default:0

let get_adv fi g =
  Gmap.find fi.i_advs g
  |> Option.value ~default:0

let get_kern fi g g' =
  try Gmap.find_exn (Gmap.find_exn fi.i_kern g) g'  with Caml.Not_found -> 0

let otf_kern_layout fi size text =
  let u_to_em = float fi.i_units_per_em in
  let rec add (prev, gs, advs, kerns as acc) i = function
  | `Malformed _ -> add acc i (`Uchar Uutf.u_rep)
  | `Uchar u ->
      let g = get_glyph fi (Uchar.to_int u) in
      let advs = get_adv fi g :: advs in
      let kerns = if prev = -1 then kerns else (get_kern fi prev g) :: kerns in
      (g, g :: gs, advs, kerns)
  in
  let rec advances acc len advs kerns = match advs, kerns with
  | adv :: advs, k :: kerns ->
      let adv = adv + k in
      let sadv = V2.v ((size *. (float adv)) /. u_to_em) 0. in
      advances (sadv :: acc) (len + adv) advs kerns
  | adv :: [], [] -> acc, len + adv
  | _ -> assert false
  in
  let _, gs, advs, kerns = Uutf.String.fold_utf_8 add (-1, [], [], []) text in
  let advs, len = advances [] 0 (List.rev advs) (List.rev kerns) in
  gs, advs, ((size *. float len) /. u_to_em)

let renderable font info size text =
  let glyphs_rev, advances_rev, len = otf_kern_layout info size text in
  let glyphs, advances = List.rev glyphs_rev, List.rev advances_rev in
  let i =
    I.const (Color.black) >>
    I.cut_glyphs ~text ~advances font glyphs >>
    I.move V2.(0.5 * (v size size))
  in
  let size = Size2.v (len +. size) (2. *. size) in
  let view = Box2.v P2.o size in
  `Image (size, view, i)

let fname, info =
  match font_info "/home/pveber/R/x86_64-pc-linux-gnu-library/3.3/shiny/www/shared/font-awesome/fonts/FontAwesome.otf" with
  | Ok fi -> fi
  | Error _ -> assert false

let text_width size text =
  let glyphs_rev, advances_rev, len = otf_kern_layout info size text in
  len
