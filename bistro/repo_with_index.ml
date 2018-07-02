(* open Core_kernel
 * open Bistro.Std
 * 
 * module Term = Bistro_utils.Term
 * module Repo = Bistro_utils.Repo
 * module H = Tyxml_html
 * 
 * type 'a path = Path of string list
 * 
 * type 'a t =
 *   | Pure : 'a -> 'a t
 *   | PureW : string list * 'a workflow -> 'a path t
 *   | App : ('a -> 'b) t * 'a t -> 'b t
 *   | List : 'a t list -> 'a list t
 * 
 * let pure x = Pure x
 * 
 * let pureW p w = PureW (p, w)
 * 
 * let app f x = App (f, x)
 * 
 * let ( $ ) = app
 * let ( $> ) x f = pure f $ x
 * 
 * let list xs = List xs
 * 
 * module Let_syntax = struct
 *   type nonrec 'a t = 'a t
 *   let map x ~f = app (pure f) x
 *   let both x y =
 *     pure (fun x y -> x, y) $ x $ y
 * end
 * 
 * let link p w text =
 *   let url = Bistro.Path.to_string p in
 *   pureW p w
 *   $> fun (Path _) -> H.(
 *       a ~a:[a_href url] [ pcdata text ]
 *     )
 * 
 * let rec to_bistro_repo : type s. Repo.t -> s t -> Repo.t = fun accu -> function
 *   | Pure _ -> accu
 *   | PureW (p, w) -> Repo.(p %> w) :: accu
 *   | App (f, x) ->
 *     to_bistro_repo
 *       (to_bistro_repo accu f)
 *       x
 *   | List xs -> List.fold_left ~f:to_bistro_repo ~init:accu xs
 * 
 * 
 * let rec to_term : type s. s t -> s Term.t = function
 *   | Pure x -> Term.pure x
 *   | PureW (p, w) ->
 *     let p = Path p in
 *     Term.(pure (fun _ -> p) $ pureW w)
 *   | App (f, x) -> Term.app (to_term f) (to_term x)
 *   | List xs ->
 *     Term.list (List.map ~f:to_term xs)
 * 
 * let use t (Bistro.Any_workflow w) =
 *   Term.(pure (fun x _ -> x) $ t $ pureW w)
 * 
 * let save_html path doc =
 *   let buf = Buffer.create 253 in
 *   let formatter = Format.formatter_of_buffer buf in
 *   Tyxml_html.pp () formatter doc ;
 *   Out_channel.with_file path ~f:(fun oc ->
 *       let contents = Buffer.contents buf in
 *       Out_channel.output_string oc contents
 *     )
 * 
 * let to_term repo ~outdir ~webroot:_ ~precious =
 *   let y = Repo.to_term ~outdir (to_bistro_repo [] repo) in
 *   let g () html =
 *     save_html
 *       (Filename.concat outdir "index.html")
 *       html
 *  in
 *  Term.(pure g $ y $ (to_term repo))
 *  |> fun init -> List.fold precious ~init ~f:use *)
