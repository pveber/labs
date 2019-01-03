open Bistro
open Bistro_bioinfo

val img : Shell_dsl.container_image list

val markduplicates :
  ?remove_duplicates:bool ->
  [`indexed_bam] dworkflow ->
  [`picard_markduplicates] dworkflow

val reads :
  [`picard_markduplicates] dworkflow ->
  bam pworkflow
