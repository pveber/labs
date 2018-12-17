open Bistro
open Bistro_bioinfo

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

val env : Shell_dsl.docker_image
val index : fasta pworkflow list -> index pworkflow
val quant :
  ?bootstrap_samples:int ->
  ?threads:int ->
  ?fragment_length:float ->
  ?sd:float ->
  index pworkflow ->
  fq1:[`fq of sanger_fastq pworkflow | `fq_gz of sanger_fastq gz pworkflow] ->
  ?fq2:[`fq of sanger_fastq pworkflow | `fq_gz of sanger_fastq gz pworkflow] ->
  unit ->
  [`kallisto_output] dworkflow

val abundance : [`kallisto_output] dworkflow -> abundance_table pworkflow

val merge_eff_counts :
  sample_ids:string list ->
  kallisto_outputs:abundance_table pworkflow list ->
  tsv pworkflow

val merge_tpms :
  sample_ids:string list ->
  kallisto_outputs:abundance_table pworkflow list ->
  tsv pworkflow
