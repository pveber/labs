open Labs_extensible_bistro

type t =
  | Input of {
      id : string ;
    }
  | Select of {
      id : string ;
    }
  | Shell of {
      id : string ;
      descr : string ;
      np : int ;
      mem : int ;
      cmd : string Command.t
    }
  | Value of {
      id : string ;
    }

module Outcome = struct
  type t =
    | Input of { path : string ; pass : bool }
    | Select of { dir_path : string ; sel : Lpath.t ; pass : bool }
    | Shell of Shell_task.outcome

  let succeeded = function
    | Input { pass } -> pass
    | Select { pass } -> pass
    | Shell o -> o.Shell_task.status = `Succeeded
end

let id = function
  | Input { id }
  | Select { id }
  | Shell { id }
  | Value {id } -> id

let of_input ~id = Input { id }
let of_shell db ~id ~descr ~np ~mem ~cmd =
  let cmd = Command.map cmd ~f:(fun (Workflow.Path_dep w) ->
      Db.cache db (Workflow.id w)
    ) in
  Shell { id ; descr ; np ; mem ; cmd }

let of_select ~id = Select { id }
let of_value ~id = Value { id }

let requirement = function
  | Input _
  | Select _ -> Allocator.Request { np = 0 ; mem = 0 }
  | Shell { np ; mem } -> Allocator.Request { np ; mem }
  | Value _ -> assert false

let perform _ = assert false
