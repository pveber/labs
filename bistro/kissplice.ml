open Core
open Bistro
open Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"kissplice" ~tag:"2.4.0" () ]

let kissplice ?(max_memory = `GB 4) ~k fq1 fq2 =
  let `GB max_mem = max_memory in
  Workflow.shell ~descr:"kissplice" ~np:8 ~mem:(Workflow.int (max_mem * 1024)) [
    mkdir_p dest ;
    cmd "kissplice" ~img [
      opt "-r" dep fq1 ;
      opt "-r" dep fq2 ;
      opt "-k" int k ;
      opt "-o" ident dest ;
      opt "-d" ident tmp ;
      opt "-t" ident np ;
      opt "--max-memory" ident mem ;
    ]
  ]
