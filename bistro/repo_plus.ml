open Core
open Bistro.Std
open Bistro_utils

module H = Tyxml_html

module Term_plus = struct
  module PathMap = Map.Make(Bistro.Path)
  type env = Bistro.any_workflow PathMap.t
  type 'a t = 'a Term.t * env

  let merge e1 e2 =
    PathMap.merge e1 e2 ~f:Bistro.(fun ~key -> function
        | `Both (Any_workflow w1 as r, Any_workflow w2) ->
          if Workflow.(id w1 = id w2) then Some r
          else
            failwithf
              "Same path for two different workflows: %s"
              (Bistro.Path.to_string key) ()
        | `Left w
        | `Right w -> Some w
      )

  let pure x = Term.pure x, PathMap.empty
  let pureW p w = Term.pureW w, PathMap.singleton p (Bistro.Any_workflow w)
  let app (f, env_f) (x, env_x) =
    Term.app f x, merge env_f env_x
  let ( $ ) = app
  let ( $> ) x f = pure f $ x
  let list xs =
    let terms, envs = List.unzip xs in
    Term.list terms, List.fold envs ~init:PathMap.empty ~f:merge

  let link p w text =
    let url = Bistro.Path.to_string p in
    pureW p w
    $> fun (Path _) -> H.(
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
  | Repo_html_page : string list * Tyxml_html.doc Term_plus.t -> item

type t = item list

let item p w = Repo_item (p, w)
let html_page p d = Repo_html_page (p, d)

let add_prefix prefix items =
  List.map items ~f:(function
      | Repo_item (p, w) -> Repo_item (prefix @ p, w)
      | Repo_html_page (p, app) -> Repo_html_page (prefix @ p, app)
    )

let save_html path doc =
  let buf = Buffer.create 253 in
  let formatter = Format.formatter_of_buffer buf in
  Unix.mkdir_p (Filename.dirname path) ;
  Tyxml_html.pp () formatter doc ;
  Out_channel.with_file path ~f:(fun oc ->
      let contents = Buffer.contents buf in
      Out_channel.output_string oc contents
    )

let to_term ?precious ~outdir items =
  let open Term in
  let normal_repo =
    List.concat_map items ~f:(function
        | Repo_html_page (_, (_, env)) ->
          Map.to_alist env
          |> List.map ~f:Repo.(fun (p, Bistro.Any_workflow w) -> p %> w)
        | Repo_item (p, w) ->
          Repo.[ p %> w ]
      )
  in
  let page_generation_term =
    List.filter_map items ~f:(function
        | Repo_item _ -> None
        | Repo_html_page (p, (term,_)) ->
          let dest =
            Filename.concat outdir (Bistro.Path.to_string p)
          in
          Some (
            pure save_html
            $ pure dest
            $ term
          )
      )
    |> list
  in
  pure (fun () _ -> ())
  $ Repo.to_term ?precious ~outdir normal_repo
  $ page_generation_term
