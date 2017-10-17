open Bistro.Std
open Bistro_bioinfo.Std

val gff_of_bed : #bed4 workflow -> gff workflow
val tss_bed_of_gff :
  upstream:int ->
  downstream:int ->
  gff workflow ->
  bed3 workflow
