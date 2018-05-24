open Bistro.Std
open Bistro_bioinfo.Std

class type index = object
  inherit binary_file
  method format : [`kallisto_index]
end

class type abundance_table = object
  inherit tsv
  method f1 : [`target_id] * string
  method f2 : [`length] * int
  method f3 : [`eff_length] * int
  method f4 : [`est_counts] * float
  method f5 : [`tpm] * float
end

val env : docker_image
val index : fasta workflow list -> index workflow
val quant :
  ?bootstrap_samples:int ->
  ?threads:int ->
  index workflow ->
  [`fq of 'a fastq workflow | `fq_gz of 'a fastq gz workflow] ->
  [`fq of 'a fastq workflow | `fq_gz of 'a fastq gz workflow] ->
  [`kallisto_output] directory workflow

val abundance : ([`kallisto_output], abundance_table) selector

val merge_eff_counts :
  sample_ids:string list ->
  kallisto_outputs:abundance_table workflow list ->
  tsv workflow

val merge_tpms :
  sample_ids:string list ->
  kallisto_outputs:abundance_table workflow list ->
  tsv workflow
