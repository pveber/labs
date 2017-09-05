open Core_kernel
open Bistro.Std

module App = Bistro_utils.Bistro_app
module Repo = Bistro_utils.Bistro_repo
module H = Tyxml_html

type 'a path = Path of string list

type 'a t =
  | Pure : 'a -> 'a t
  | PureW : string list * 'a workflow -> 'a path t
  | App : ('a -> 'b) t * 'a t -> 'b t
  | List : 'a t list -> 'a list t

let pure x = Pure x

let pureW p w = PureW (p, w)

let app f x = App (f, x)

let ( $ ) = app
let ( $> ) x f = pure f $ x

let list xs = List xs

module Syntax = struct
  module Let_syntax = struct
    type nonrec 'a t = 'a t
    let map x ~f = app (pure f) x
    let both x y =
      pure (fun x y -> x, y) $ x $ y
  end
end

let link p w text =
  let url = Bistro.Path.to_string p in
  pureW p w
  $> fun (Path p) -> H.(
      a ~a:[a_href url] [ pcdata text ]
    )

let rec to_bistro_repo : type s. Repo.t -> s t -> Repo.t = fun accu -> function
  | Pure _ -> accu
  | PureW (p, w) -> Repo.(p %> w) :: accu
  | App (f, x) ->
    to_bistro_repo
      (to_bistro_repo accu f)
      x
  | List xs -> List.fold_left ~f:to_bistro_repo ~init:accu xs


let rec to_app : type s. s t -> s App.t = function
  | Pure x -> App.pure x
  | PureW (p, w) ->
    let p = Path p in
    App.(pure (fun _ -> p) $ pureW w)
  | App (f, x) -> App.app (to_app f) (to_app x)
  | List xs ->
    App.list (List.map ~f:to_app xs)

let use t (Bistro.Any_workflow w) =
  App.(pure (fun x _ -> x) $ t $ pureW w)

let save_html path doc =
  let buf = Buffer.create 253 in
  let formatter = Format.formatter_of_buffer buf in
  Tyxml_html.pp () formatter doc ;
  Out_channel.with_file path ~f:(fun oc ->
      let contents = Buffer.contents buf in
      Out_channel.output_string oc contents
    )

let to_app repo ~outdir ~webroot ~precious =
  let y = Repo.to_app ~outdir (to_bistro_repo [] repo) in
  let g () html =
    save_html
      (Filename.concat outdir "index.html")
      html
 in
 App.(pure g $ y $ (to_app repo))
 |> fun init -> List.fold precious ~init ~f:use
