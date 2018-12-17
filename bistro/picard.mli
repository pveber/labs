open Bistro
open Bistro_bioinfo

val docker_image : Shell_dsl.docker_image

val markduplicates :
  ?remove_duplicates:bool ->
  [`indexed_bam] dworkflow ->
  [`picard_markduplicates] dworkflow

val reads :
  [`picard_markduplicates] dworkflow ->
  bam pworkflow
