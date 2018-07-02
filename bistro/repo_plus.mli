(* open Bistro.Std
 * open Bistro_utils
 * 
 * type item
 * type t = item list
 * 
 * module Term_plus : sig
 *   type 'a t
 * 
 *   val pure : 'a -> 'a t
 *   val pureW : Bistro.Path.t -> 'a Bistro.Std.workflow -> 'a Term.path t
 *   val app : ('a -> 'b) t -> 'a t -> 'b t
 *   val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t
 *   val list : 'a t list -> 'a list t
 * 
 *   val link :
 *     Bistro.Path.t ->
 *     'a workflow ->
 *     string ->
 *     [> `A of [> `PCDATA ] ] Tyxml_html.elt t
 * 
 *   module Let_syntax : sig
 *     type nonrec 'a t = 'a t
 *     val map  : 'a t -> f:('a -> 'b) -> 'b t
 *     val both : 'a t -> 'b t -> ('a * 'b) t
 *   end
 * end
 * 
 * val item : string list -> _ workflow -> item
 * val html_page : string list -> Tyxml_html.doc Term_plus.t -> item
 * 
 * val add_prefix : string list -> t -> t
 * 
 * val to_term :
 *   ?precious:Bistro.any_workflow list ->
 *   outdir:string ->
 *   t ->
 *   unit Term.t *)
