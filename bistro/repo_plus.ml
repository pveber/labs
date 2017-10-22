open Core
open Bistro.Std
open Bistro_utils
open Bistro_engine

module H = Tyxml_html

module Term = struct
  module App = Bistro_app
  module PathMap = Map.Make(Bistro.Path)
  type env = Bistro.u PathMap.t
  type 'a t = 'a App.t * env

  let merge e1 e2 =
    PathMap.merge e1 e2 ~f:(fun ~key -> function
        | `Both (w1, w2) ->
          if Bistro.U.(id w1 = id w2) then Some w1
          else
            failwithf
              "Same path for two different workflows: %s"
              (Bistro.Path.to_string key) ()
        | `Left w
        | `Right w -> Some w
      )

  let pure x = App.pure x, PathMap.empty
  let pureW p w = App.pureW w, PathMap.singleton p (Bistro.Workflow.u w)
  let app (f, env_f) (x, env_x) =
    App.app f x, merge env_f env_x
  let ( $ ) = app
  let ( $> ) x f = pure f $ x
  let list xs =
    let terms, envs = List.unzip xs in
    App.list terms, List.fold envs ~init:PathMap.empty ~f:merge

  let link p w text =
    let url = Bistro.Path.to_string p in
    pureW p w
    $> fun (Path p) -> H.(
        a ~a:[a_href url] [ pcdata text ]
      )

  module Let_syntax = struct
    type nonrec 'a t = 'a t
    let map x ~f = app (pure f) x
    let both x y =
      pure (fun x y -> x, y) $ x $ y
  end
end

type item =
  | Repo_item : string list * _ workflow -> item
  | Repo_html_page : string list * Tyxml_html.doc Bistro_app.t -> item

type t = item list

let item p w = Repo_item (p, w)
let html_page p (d, _) = Repo_html_page (p, d)

let add_prefix prefix items =
  List.map items ~f:(function
      | Repo_item (p, w) -> Repo_item (prefix @ p, w)
      | Repo_html_page (p, app) -> Repo_html_page (prefix @ p, app)
    )

let save_html path doc =
  let buf = Buffer.create 253 in
  let formatter = Format.formatter_of_buffer buf in
  Tyxml_html.pp () formatter doc ;
  Out_channel.with_file path ~f:(fun oc ->
      let contents = Buffer.contents buf in
      Out_channel.output_string oc contents
    )

let to_app ?precious ~outdir items =
  let open Bistro_app in
  let normal_repo =
    List.filter_map items ~f:(function
        | Repo_html_page _ -> None
        | Repo_item (p, w) ->
          Some Bistro_repo.(p %> w)
      )
  in
  let page_generation_term =
    List.filter_map items ~f:(function
        | Repo_item _ -> None
        | Repo_html_page (p, d) ->
          Some (
            pure save_html
            $ pure (Bistro.Path.to_string p)
            $ d
          )
      )
    |> list
  in
  pure (fun () _ -> ())
  $ Bistro_repo.to_app ?precious ~outdir normal_repo
  $ page_generation_term
