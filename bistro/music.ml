open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"music" ~tag:"6613c5" ()

let prepare x label =
  [
    cmd "MUSIC" [
      string "-preprocess" ;
      dep x ;
      tmp // label ;
    ] ;
    cmd "MUSIC" [
      string "-sort_reads" ;
      tmp // label ;
      tmp // label // "sorted" ;
    ] ;
    cmd "MUSIC" [
      string "-remove_duplicates" ;
      tmp // label // "sorted" ;
      int 2 ;
      tmp // label // "dedup" ;
    ] ;

  ]

let run ~mappability ~l_mapp ~begin_l ~end_l ~step ~treatment ~control =
  workflow ~descr:"music" [
    and_list (prepare treatment "treatment") ;
    and_list (prepare control "control") ;
    mkdir_p dest ;
    and_list [
      cd dest ;
      cmd "MUSIC" [
        string "-get_multiscale_broad_ERs" ;
        string "-chip" ; tmp // "treatment" // "dedup" ;
        string "-control" ; tmp // "control" // "dedup" ;
        opt "-mapp" dep mappability ;
        opt "-l_mapp" int l_mapp ;
        opt "-begin_l" int begin_l ;
        opt "-end_l" int end_l ;
        opt "-step" float step ;
      ] ;
    ] ;
  ]
