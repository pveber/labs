open Bistro.Std
open Bistro_bioinfo.Std

type 'a format

val narrowPeak : Macs2.narrow_peaks format
val broadPeak : Macs2.broad_peaks format
val bed : bed3 format
val gff : gff format

type output = [`idr_output]

val idr :
  input_file_type:'a format ->
  ?idr_threshold:float ->
  ?soft_idr_threshold:float ->
  ?peak_merge_method:[ `sum | `avg | `min | `max] ->
  ?random_seed:int ->
  ?peak_list:'a workflow ->
  'a workflow list ->
  output directory workflow

val items : (output, bed5) selector
val figure : (output, png) selector
