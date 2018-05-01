(**
   http://subread.sourceforge.net/
*)

open Bistro.Std
open Bistro_bioinfo.Std

class type count_table = object
  inherit tsv
  method header : [`none]
  method f1 : string
  method f2 : int
end

val featureCounts :
  ?feature_type:string ->
  ?attribute_type:string ->
  ?strandness:[`Unstranded | `Stranded | `Reversely_stranded] ->
  ?nthreads:int ->
  gff workflow ->
  < format : [< `bam | `sam] ; .. > workflow -> (*FIXME: handle paired-hand, just add other file next to the other*)
  [`featureCounts] directory workflow

val featureCounts_tsv : ([`featureCounts], count_table) selector
val featureCounts_summary : ([`featureCounts], text_file) selector
