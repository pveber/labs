open Bistro.Std
open Bistro_bioinfo.Std

val docker_image : docker_image

val markduplicates :
  [`indexed_bam] directory workflow ->
  [`picard_markduplicates] directory workflow
