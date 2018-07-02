(* open Bistro_utils
 * 
 * type 'a path = private Path of string list
 * type 'a t
 * 
 * val pure : 'a -> 'a t
 * val pureW : Bistro.Path.t -> 'a Bistro.Std.workflow -> 'a path t
 * val app : ('a -> 'b) t -> 'a t -> 'b t
 * val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t
 * val list : 'a t list -> 'a list t
 * 
 * val link :
 *   Bistro.Path.t ->
 *   'a Bistro.Std.workflow ->
 *   string ->
 *   [> `A of [> `PCDATA ] ] Tyxml_html.elt t
 * 
 * val to_term :
 *   Tyxml_html.doc t ->
 *   outdir:string ->
 *   webroot:string ->
 *   precious:Bistro.any_workflow list ->
 *   unit Term.t
 * 
 * module Let_syntax : sig
 *   type nonrec 'a t = 'a t
 *   val map  : 'a t -> f:('a -> 'b) -> 'b t
 *   val both : 'a t -> 'b t -> ('a * 'b) t
 * end *)
