open Bistro
open Bistro_bioinfo

val gff_of_bed : #bed4 pworkflow -> gff pworkflow
val tss_bed_of_gff :
  upstream:int ->
  downstream:int ->
  gff pworkflow ->
  bed4 pworkflow
