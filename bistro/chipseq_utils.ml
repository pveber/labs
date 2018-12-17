open Base

let%pworkflow gff_of_bed bed =
  let open Biocaml_base in
  let open Labs_biopipes.Biopipes in
  run (
    from_file [%path bed]
    $$ bed_parser ()
    $$ map (function
        | (chr, start_pos, stop_pos, name :: _) ->
          `Record (
            Gff.record
              ~strand:`Not_stranded
              ~attributes:["peak_id", [name]]
              ~feature:"peak"
              chr start_pos stop_pos
          )
        | _ -> failwith "gff_of_bed: expect at least bed4"
      )
    $$ gff_unparser `two
    $$ to_file [%dest]
  )

let%pworkflow tss_bed_of_gff ~upstream ~downstream gff =
  let open Biocaml_base in
  let open Labs_biopipes.Biopipes in
  run (
    from_file [%path gff]
    $$ gff3_parser ()
    $$ filter_map (function
        | `Record { Gff.feature = Some "mRNA" ;
                    seqname ; start_pos ; stop_pos ; strand ; attributes ; _ } ->
          let start_pos, stop_pos =
            match strand with
            | `Plus -> start_pos - upstream, start_pos + downstream
            | `Minus -> stop_pos - downstream, stop_pos + upstream
            | `Not_stranded
            | `Unknown -> assert false
          in
          let name =
            match List.Assoc.find attributes "transcript_id" ~equal:String.equal with
            | Some [ tid ] -> tid
            | Some (_ :: _) -> failwith "Unexpected multiple values for transcript id"
            | Some []
            | None -> failwith "Missing value for transcript id"
          in
          Some (seqname, start_pos, stop_pos, [ name ])
        | _ -> None
      )
    $$ bed_unparser ()
    $$ to_file [%dest]
  )
