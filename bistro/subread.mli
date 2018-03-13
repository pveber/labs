open Bistro.Std
open Bistro_bioinfo.Std

class type count_table = object
  inherit tsv
  method header : [`none]
  method f1 : string
  method f2 : int
end

val featureCounts :
  gff workflow ->
  < format : [< `bam | `sam] ; .. > workflow -> (*FIXME: handle paired-hand, just add other file next to the other*)
  count_table workflow

