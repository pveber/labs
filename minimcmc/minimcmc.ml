(* module type Repr = sig
 *   type 'a t
 * 
 *   val pure : 'a -> 'a t
 *   val app : ('a -> 'b) t -> 'a t -> 'b t
 * 
 *   val poisson : float t -> int t
 *   val normal :
 *     mu:float t ->
 *     sigma:float t ->
 *     float t
 * 
 *   val observe : 'a t -> 'a -> 'a t
 * end
 * 
 * module PoissonLogNormal(R : Repr) = struct
 *   open R
 * 
 *   let f () =
 *     let mu = normal ~mu:(pure 0.) ~sigma:(pure 1.) in
 *     poisson (app (pure (( ** ) 10.)) mu)
 * end
 * 
 * 
 * module type Repr2 = sig
 *   type 'a t
 *   type 'a dist
 * 
 *   val pure : 'a -> 'a t
 *   val app : ('a -> 'b) t -> 'a t -> 'b t
 *   val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t
 * 
 *   val sample :
 *     ?obs:'a ->
 *     'a dist ->
 *     'a t
 * 
 *   val poisson : float -> int dist
 *   val normal :
 *     mu:float ->
 *     sigma:float ->
 *     float dist
 * 
 * end
 * 
 * module PoissonLogNormal(R : Repr2) = struct
 *   open R
 * 
 *   let f k =
 *     let mu = sample (normal ~mu:0. ~sigma:1.) in
 *     pure (fun x -> sample ~obs:k (poisson x)) $ mu
 * end
 * 
 * module type Syntax = sig
 *   type 'a t
 *   type 'a rv
 *   type 'a dist
 * 
 *   val const : 'a -> 'a rv
 *   val ( >> ) : 'a rv t -> ('a -> 'b) -> 'b rv t
 *   val ( >>= ) : 'a rv t -> ('a rv -> 'b rv t) -> 'b rv t
 * 
 *   val sample :
 *     ?obs:'a ->
 *     'a dist ->
 *     'a rv t
 * 
 *   val poisson : float rv -> int dist
 *   val beta : alpha:float rv -> beta:float rv -> float dist
 *   val normal : mu:float rv -> sigma:float rv -> float dist
 * end
 * 
 * module Models(S : Syntax) = struct
 *   open S
 * 
 *   let poisson_beta k =
 *     sample (beta ~alpha:(const 1.) ~beta:(const 1.)) >>= fun lambda ->
 *     sample ~obs:k (poisson lambda)
 * 
 *   let poisson_log_normal k =
 *     sample (normal ~mu:(const 0.) ~sigma:(const 1.)) >> (( ** ) 10.) >>= fun lambda ->
 *     sample ~obs:k (poisson lambda)
 * 
 *   (\* let wrong =
 *    *   sample (normal ~mu:(const 0.) ~sigma:(const 1.)) >>= fun x ->
 *    *   sample ~obs:x (normal ~mu:(const 0.) ~sigma:(const 1.)) *\)
 * end
 * 
 * 
 * module type Syntax = sig
 *   type 'a t
 *   type 'a rv
 *   type 'a dist
 * 
 *   val const : 'a -> 'a rv
 *   val map : 'a rv t -> f:('a -> 'b rv) -> 'b rv t
 *   val map2 :
 *     'a rv t ->
 *     'b rv t ->
 *     f:('a -> 'b -> 'c rv) -> 'c rv t
 * 
 *   val sample :
 *     ?obs:'a ->
 *     'a dist ->
 *     'a rv
 * 
 *   val poisson : float -> int dist
 *   val beta : alpha:float -> beta:float -> float dist
 *   val normal : mu:float -> sigma:float -> float dist
 * end
 * 
 * module type Syntax = sig
 *   type 'a rv
 *   type 'a dist
 * 
 *   val const : 'a -> 'a rv
 *   val ( >> ) : 'a rv -> ('a -> 'b) -> 'b rv
 *   val ( $ ) : ('a -> 'b) rv -> 'a rv -> 'b rv
 * 
 *   val sample :
 *     ?obs:'a ->
 *     'a dist rv ->
 *     'a rv
 * 
 *   val poisson : float -> int dist
 *   val beta : alpha:float -> beta:float -> float dist
 *   val normal : mu:float -> sigma:float -> float dist
 * end
 * 
 * module Models(S : Syntax) = struct
 *   open S
 * 
 *   let poisson_beta k =
 *     let lambda = sample (const @@ beta ~alpha:1. ~beta:1.) in
 *     sample ~obs:k (lambda >> poisson)
 * 
 *   let poisson_log_normal k_obs =
 *     let log_lambda = sample (const @@ normal ~mu:0. ~sigma:1.) in
 *     sample ~obs:k_obs (log_lambda >> ( ** ) 10. >> poisson)
 * 
 *   (\* let wrong =
 *    *   let x = sample (const @@ normal ~mu:0. ~sigma:1.) in
 *    *   sample ~obs:x (const @@ normal ~mu:0. ~sigma:1.) *\)
 * 
 *   let hierarchical_poisson_log_normal counts =
 *     let log_lambda_prior_mu = sample (const @@ normal ~mu:0. ~sigma:1.) in
 *     let log_lambda_prior_sigma = sample (const @@ beta ~alpha:1. ~beta:1.) >> (( /. ) 1.) in
 *     let gene_count k_obs =
 *       let log_lambda = sample (const (fun mu sigma -> normal ~mu ~sigma) $ log_lambda_prior_mu $ log_lambda_prior_sigma) in
 *       sample ~obs:k_obs (log_lambda >> ( ** ) 10. >> poisson)
 *     in
 *     Array.map gene_count counts
 * 
 *   module Let_syntax = struct
 *     let map x ~f = x >> f
 *     let both x y = const (fun x y -> x, y) $ x $ y
 *   end
 * 
 *   let hierarchical_poisson_log_normal counts =
 *     let log_lambda_prior_mu = sample (const @@ normal ~mu:0. ~sigma:1.) in
 *     let log_lambda_prior_sigma = sample (const @@ beta ~alpha:1. ~beta:1.) >> (( /. ) 1.) in
 *     let gene_count k_obs =
 *       let log_lambda =
 *         sample (
 *           let%map mu = log_lambda_prior_mu
 *           and sigma = log_lambda_prior_sigma in
 *           normal ~mu ~sigma
 *         )
 *       in
 *       sample ~obs:k_obs (log_lambda >> ( ** ) 10. >> poisson)
 *     in
 *     Array.map gene_count counts
 * 
 * end *)





module type Syntax = sig
  type 'a rv
  type 'a dist

  val const : 'a -> 'a rv
  val ( >> ) : 'a rv -> ('a -> 'b) -> 'b rv
  val ( $ ) : ('a -> 'b) rv -> 'a rv -> 'b rv

  val sample :
    ?obs:'a ->
    'a dist rv ->
    'a rv

  val poisson : float rv -> int dist rv
  val beta : alpha:float rv -> beta:float rv -> float dist rv
  val normal : mu:float rv -> sigma:float rv -> float dist rv
end

module Models(S : Syntax) = struct
  open S

  let poisson_beta k =
    let lambda = sample (beta ~alpha:(const 1.) ~beta:(const 1.)) in
    sample ~obs:k (poisson lambda)

  let poisson_log_normal k_obs =
    let log_lambda = sample (normal ~mu:(const 0.) ~sigma:(const 1.)) in
    sample ~obs:k_obs (poisson (log_lambda >> ( ** ) 10. ))

  (* let wrong =
   *   let x = sample (normal ~mu:(const 0.) ~sigma:(const 1.)) in
   *   sample ~obs:x (normal ~mu:(const 0.) ~sigma:(const 1.)) *)

  let hierarchical_poisson_log_normal counts =
    let log_lambda_prior_mu = sample (normal ~mu:(const 0.) ~sigma:(const 1.)) in
    let log_lambda_prior_sigma = sample (beta ~alpha:(const 1.) ~beta:(const 1.)) >> (( /. ) 1.) in
    let gene_count k_obs =
      let log_lambda = sample (normal ~mu:log_lambda_prior_mu ~sigma:log_lambda_prior_sigma) in
      sample ~obs:k_obs (poisson (log_lambda >> ( ** ) 10.))
    in
    Array.map gene_count counts

end



module DAG = struct
  type node =
    | Const
    | Node of {
        id : int ;
        parents : node list ;
      }

  type dag =
    | DAG of {
        head : node ;
        tail : node list ;
      }

  type 'a rv = dag -> dag

  type 'a dist = Dist of {
      name : string ;
      sample : unit -> 'a ;
      density : 'a -> float ;
    }

  let new_id =
    let c = ref 0 in
    fun () -> incr c ; !c

  let const _ = fun dag ->
    let node = Const in
    DAG {
      head = node ;
      tail = [] ;
    }

  let ( >> ) cons f = fun dag ->
    let DAG d = cons dag in
    let head =
      Node {
        id = new_id () ;
        parents = [ d.head ] ;
      }
    in
    DAG {
      head ;
      tail = d.head :: d.tail ;
    }

end
