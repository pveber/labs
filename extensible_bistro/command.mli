type docker_image = {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}
[@@deriving sexp]

type 'a t =
  | Docker of docker_image * 'a t
  | Simple_command of 'a Template.t
  | And_list of 'a t list
  | Or_list of 'a t list
  | Pipe_list of 'a t list

val deps : 'a t -> 'a list
val map : 'a t -> f:('a -> 'b) -> 'b t
