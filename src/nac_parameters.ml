
open Printf

type t =
  {
    mutable parallelization_mode : Parallelization_mode.t;
    
    mutable classification_mode : Classification_mode.t;

    mutable packet_parsing_mode : Packet_parsing_mode.t;
    mutable match_timestamps : bool;
    mutable build_all_stat : bool;
    mutable check_five_tuple_flow_metrics_timestamp : bool;
    (* mutable date_format_string : string; *)
    (* mutable time_format_string : string; *)
    (* mutable default_hour_minute_second : (int * int * int) option; *)    
    mutable export_values_attributes : bool;

    mutable taxonomy_filepath : string;
  }

let new_t    
    parallelization_mode

    classification_mode

    packet_parsing_mode
    match_timestamps
    build_all_stat
    check_five_tuple_flow_metrics_timestamp
    export_values_attributes

    taxonomy_filepath
  =
  {
    parallelization_mode;

    classification_mode;

    packet_parsing_mode;
    match_timestamps;
    build_all_stat;
    check_five_tuple_flow_metrics_timestamp;
    export_values_attributes;

    taxonomy_filepath;
  }
  
let new_empty_t () =
  new_t
    Parallelization_mode.No_parallelization
    
    Classification_mode.Not_defined

    Packet_parsing_mode.IPV4
    false
    false
    true    
    false
    
    ""

let to_string t =
  sprintf
    "nac parameters:\nParallelization_mode: %s\nClassification mode: %s\npacket parsing mode: %s\nMatch timestamps: %b\nBuild all stat: %b\nCheck_five_tuple_flow_metrics_timestamp: %b\nExport_metrics_attributes: %b\ntaxonomy_filepath: %s"
    (Parallelization_mode.to_string t.parallelization_mode)

    (Classification_mode.to_string t.classification_mode)

    (Packet_parsing_mode.to_string t.packet_parsing_mode)
    t.match_timestamps
    t.build_all_stat
    t.check_five_tuple_flow_metrics_timestamp
    t.export_values_attributes

    t.taxonomy_filepath

let check t =
  (
    (
      t.classification_mode <> Classification_mode.Not_defined
      &&
      t.taxonomy_filepath <> ""
    )
    &&
    (
      if t.match_timestamps then
        t.build_all_stat = false
      else
        true
    )
  )
