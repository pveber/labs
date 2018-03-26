type _ term =
  | Pure : 'a -> 'a term
  | Get  : 'a key -> 'a term
  | App  : ('a -> 'b) term * 'a term -> 'b term

type 'a t = {
  id : int ;
  term : 'a term ;
}
