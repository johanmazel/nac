
open Printf

type t =
  {
    mutable classification_mode : Classification_mode.t;

    mutable taxonomy_filepath : string;

    mutable packet_parsing_mode : Packet_parsing_mode.t;

    (* mutable date_format_string : string; *)
    (* mutable time_format_string : string; *)
    (* mutable default_hour_minute_second : (int * int * int) option; *)

    mutable match_timestamps : bool;
    mutable export_values_attributes : bool;

    mutable parallelization_mode : Parallelization_mode.t;
  }

let new_t
    classification_mode
    
    taxonomy_filepath
    
    packet_parsing_mode
    match_timestamps
    export_values_attributes
    
    parallelization_mode
  =
  {
    classification_mode;

    taxonomy_filepath;

    packet_parsing_mode;
    match_timestamps;
    export_values_attributes;

    parallelization_mode;
  }
  
let new_empty_t () =
  new_t
    Classification_mode.Not_defined

    ""

    Packet_parsing_mode.IPV4
    false
    false

    Parallelization_mode.No_parallelization

let to_string t =
  sprintf
    "nac parameters:\nClassification mode: %s\ntaxonomy_filepath: %s\npacket parsing mode: %s\nMatch timestamps: %b\nExport_metrics_attributes: %b"
    (Classification_mode.to_string t.classification_mode)
    t.taxonomy_filepath
    (Packet_parsing_mode.to_string t.packet_parsing_mode)
    t.match_timestamps
    t.export_values_attributes

let check t =
  t.classification_mode <> Classification_mode.Not_defined
  &&
  t.taxonomy_filepath <> ""
