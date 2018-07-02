module T = Bistro.Template_dsl

let int = T.int

let string = T.string

let list f x = T.seq ~sep:" " [
    T.string "(" ;
    T.list ~sep:" " f x ;
    T.string ")" ;
  ]

let seq xs = list (fun x -> x) xs
