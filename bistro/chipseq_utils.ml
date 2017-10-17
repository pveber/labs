let%bistro gff_of_bed bed =
  let open Biocaml_base in
  let open Labs_biopipes.Biopipes in
  run (
    from_file [%dep bed]
    $$ bed_parser ()
    $$ map (function
        | (chr, start_pos, stop_pos, name :: _) ->
          `Record (
            Gff.record
              ~strand:`Not_stranded
              ~attributes:["gene_id", [name]]
              ~feature:"peak"
              chr start_pos stop_pos
          )
        | _ -> failwith "gff_of_bed: expect at least bed4"
      )
    $$ gff_unparser `two
    $$ to_file [%dest]
  )

let%bistro tss_bed_of_gff ~upstream ~downstream gff =
  let open Biocaml_base in
  let open Labs_biopipes.Biopipes in
  run (
    from_file [%dep gff]
    $$ gff3_parser ()
    $$ filter_map (function
        | `Record { Gff.feature = Some "transcript" ; seqname ; start_pos ; stop_pos ; strand } ->
          let start_pos, stop_pos =
            match strand with
            | `Plus -> start_pos - upstream, start_pos + downstream
            | `Minus -> stop_pos - downstream, stop_pos + upstream
            | `Not_stranded
            | `Unknown -> assert false
          in
          Some (seqname, start_pos, stop_pos, [])
        | _ -> None
      )
    $$ bed_unparser ()
    $$ to_file [%dest]
  )
