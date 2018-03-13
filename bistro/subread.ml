open Core
open Bistro.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"subread" ~tag:"1.6.0" ()

class type count_table = object
  inherit tsv
  method header : [`none]
  method f1 : string
  method f2 : int
end

let featureCounts gff mapped_reads =
  workflow ~descr:"featureCounts" [
    cmd "featureCounts" ~env [
      opt "-a" dep gff ;
      opt "-o" ident dest ;
      dep mapped_reads ;
    ]
  ]

