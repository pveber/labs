open Bistro
open Bistro_bioinfo

type index = [`star_index] directory

val genomeGenerate : fasta workflow -> index workflow

val alignReads :
  ?max_mem:[`GB of int] ->
  ?outFilterMismatchNmax:int ->
  ?outFilterMultimapNmax:int ->
  ?outSAMstrandField:[`None | `intronMotif] ->
  ?alignIntronMax:int ->
  index workflow ->
  [ `single_end of 'a fastq workflow
  | `paired_end of 'a fastq workflow * 'a fastq workflow ] ->
  sam workflow
