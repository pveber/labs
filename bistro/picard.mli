open Bistro
open Bistro_bioinfo

val docker_image : docker_image

val markduplicates :
  ?remove_duplicates:bool ->
  [`indexed_bam] directory workflow ->
  [`picard_markduplicates] directory workflow

val reads : ([`indexed_bam], bam) selector
