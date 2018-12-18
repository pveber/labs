open Bistro
open Bistro_bioinfo

val genomeGenerate : fasta pworkflow -> [`star_index] dworkflow

val alignReads :
  ?max_mem:[`GB of int] ->
  ?outFilterMismatchNmax:int ->
  ?outFilterMultimapNmax:int ->
  ?outSAMstrandField:[`None | `intronMotif] ->
  ?alignIntronMax:int ->
  [`star_index] dworkflow ->
  [ `single_end of sanger_fastq pworkflow
  | `paired_end of sanger_fastq pworkflow * sanger_fastq pworkflow ] ->
  bam pworkflow
