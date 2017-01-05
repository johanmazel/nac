
open Printf

module A = BatArray
module S = BatString
module HT = BatHashtbl
module L = BatList

open Admd.Instantiation
    
(* open Admd_mawilab_xml_classification_instantiation *)
(* open Admd_mawilab_binary_classification_instantiation *)

(* open Mawilab_admd_instantiations *)

open Map_ext_instantiations

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Admd_detailed_metrics_builder]: %s@." s)
      else
        ignore
    )
    fmt

(* TODO: remove *)
let process_no_timestamp__
    parallelization_mode

    packet_parsing_mode
    trace_file_path

    anomaly_container
    anomaly_slice_time_data_container
    
    filter_criteria_list
  =
  (
    let
      trace_statistics,
      five_tuple_flow_metrics_container,
      five_tuple_key_five_tuple_flow_set_container
      =
      Trace_xml_data_builder.get_trace_data_tuple
        packet_parsing_mode
        trace_file_path
        filter_criteria_list
    in

    let anomaly_detailed_metrics_container =
      Anomaly_detailed_metrics_container.of_anomaly_container_five_tuple_flow_metrics_container
        parallelization_mode

        five_tuple_flow_metrics_container
        five_tuple_key_five_tuple_flow_set_container

        (* anomaly_container *)
        anomaly_slice_time_data_container
    in

    assert(
      Base.Anomaly_container.length anomaly_container
      =
      Anomaly_detailed_metrics_container.length anomaly_detailed_metrics_container
    );

    trace_statistics, anomaly_detailed_metrics_container
  )

let process_parsing_anomaly_traffic
    parallelization_mode
    match_timestamps

    packet_parsing_mode
    trace_file_path

    (* anomaly_container_ *)
    anomaly_slice_time_data_container
  =
  (
    debug "process_timestamp: call";

    let anomaly_data_list =
      Anomaly_slice_time_data_container.to_list
        anomaly_slice_time_data_container
    in

    let five_tuple_flow_element_anomaly_indice_container =
      Five_tuple_flow_element_anomaly_indice_container.of_anomaly_data_list
        anomaly_data_list
    in

    (* debug *)
    (*   "process_timestamp: five_tuple_flow_anomaly_indice_container:\n%s" *)
    (*   (Five_tuple_flow_anomaly_indice_container.to_string *)
    (*      five_tuple_flow_anomaly_indice_container *)
    (*   ) *)
    (* ; *)

    (* assert( *)
    (*   Base.Anomaly_container.length anomaly_container *)
    (*   = *)
    (*   HT.length five_tuple_flow_element_anomaly_indice_container_ref.Five_tuple_flow_element_anomaly_indice_container_ref.anomaly_h *)
    (* ); *)
    (* assert( *)
    (*   Base.Anomaly_container.length anomaly_container *)
    (*   = *)
    (*   Five_tuple_flow_element_anomaly_indice_container.length five_tuple_flow_element_anomaly_indice_container *)
    (* ); *)

    let trace_statistics, anomaly_five_tuple_flow_metrics_container =
      Trace_anomaly_five_tuple_flow_metrics_container_builder.process
        parallelization_mode
        match_timestamps
        
        five_tuple_flow_element_anomaly_indice_container

        packet_parsing_mode
        trace_file_path
    in

    (* debug *)
    (*   "process_timestamp: :\n%s" *)
    (*   (Anomaly_five_tuple_flow_metrics_container.to_string *)
    (*      anomaly_five_tuple_flow_metrics_container *)
    (*   ) *)
    (* ; *)

    (* if *)
    (*   Base.Anomaly_container.length anomaly_container *)
    (*   <> *)
    (*   Anomaly_five_tuple_flow_metrics_container.length anomaly_five_tuple_flow_metrics_container *)
    (* then *)
    (*   ( *)
    (*     failwith *)
    (*       (sprintf *)
    (*          "process_timestamp: inconsistent length between anomaly_container (%d) and anomaly_five_tuple_flow_metrics_container %d" *)
    (*          (Base.Anomaly_container.length anomaly_container) *)
    (*          (Anomaly_five_tuple_flow_metrics_container.length anomaly_five_tuple_flow_metrics_container) *)
    (*       ) *)
    (*   ); *)

    let anomaly_detailed_metrics_container =
      Anomaly_detailed_metrics_container.of_anomaly_five_tuple_flow_metrics_container
        (* anomaly_container_ *)
        anomaly_slice_time_data_container

        anomaly_five_tuple_flow_metrics_container
    in

    if
      (* Base.Anomaly_container.length anomaly_container *)
      (Anomaly_slice_time_data_container.length anomaly_slice_time_data_container)
      <>
      Anomaly_detailed_metrics_container.length anomaly_detailed_metrics_container
    then
      (
        failwith
          (sprintf
             "process_timestamp: inconsistent length between anomaly_container (%d) and anomaly_five_tuple_flow_metrics_container %d"
             (* (Base.Anomaly_container.length anomaly_container) *)
             (Anomaly_slice_time_data_container.length anomaly_slice_time_data_container)
             (Anomaly_detailed_metrics_container.length anomaly_detailed_metrics_container)
          )
      );

    (* assert( *)
    (*   Base.Anomaly_container.length anomaly_container *)
    (*   = *)
    (*   Anomaly_detailed_metrics_container.length anomaly_detailed_metrics_container *)
    (* ); *)

    debug "process_timestamp: end";

    trace_statistics, anomaly_detailed_metrics_container
  )

let process_parsing_all_traffic
    parallelization_mode

    packet_parsing_mode
    match_timestamps
    trace_file_path

    (* anomaly_container *)
    anomaly_slice_time_data_container
  =
  (
    debug "process_timestamp: call";

    (* if *)
    (*   Base.Anomaly_container.length anomaly_container *)
    (*   <> *)
    (*   Anomaly_five_tuple_flow_metrics_container.length anomaly_five_tuple_flow_metrics_container *)
    (* then *)
    (*   ( *)
    (*     failwith *)
    (*       (sprintf *)
    (*          "process_timestamp: inconsistent length between anomaly_container (%d) and anomaly_five_tuple_flow_metrics_container %d" *)
    (*          (Base.Anomaly_container.length anomaly_container) *)
    (*          (Anomaly_five_tuple_flow_metrics_container.length anomaly_five_tuple_flow_metrics_container) *)
    (*       ) *)
    (*   ); *)

    let trace_statistics, five_tuple_flow_metrics_container =
      Trace_five_tuple_flow_metrics_container_builder.process
        packet_parsing_mode
        trace_file_path
    in

    let five_tuple_key_five_tuple_flow_set_container =
      Five_tuple_key_five_tuple_flow_set_container.of_five_tuple_flow_metrics_container
        five_tuple_flow_metrics_container
    in

    let anomaly_detailed_metrics_container =
      Anomaly_detailed_metrics_container.of_anomaly_container_five_tuple_flow_metrics_container
        parallelization_mode

        five_tuple_flow_metrics_container
        five_tuple_key_five_tuple_flow_set_container

        (* anomaly_container *)
        anomaly_slice_time_data_container
    in

    if
      (* Base.Anomaly_container.length anomaly_container *)
      (Anomaly_slice_time_data_container.length anomaly_slice_time_data_container)
      <>
      Anomaly_detailed_metrics_container.length anomaly_detailed_metrics_container
    then
      (
        failwith
          (sprintf
             "process_timestamp: inconsistent length between anomaly_container (%d) and anomaly_five_tuple_flow_metrics_container %d"
             (* (Base.Anomaly_container.length anomaly_container) *)
             (Anomaly_slice_time_data_container.length anomaly_slice_time_data_container)
             (Anomaly_detailed_metrics_container.length anomaly_detailed_metrics_container)
          )
      );

    (* assert( *)
    (*   Base.Anomaly_container.length anomaly_container *)
    (*   = *)
    (*   Anomaly_detailed_metrics_container.length anomaly_detailed_metrics_container *)
    (* ); *)

    debug "process_timestamp: end";

    trace_statistics, anomaly_detailed_metrics_container
  )

let process
    parallelization_mode

    packet_parsing_mode
    match_timestamps
    build_all_stat

    trace_file_path

    (* filter_criteria_list *)
    (* anomaly_container *)
    anomaly_slice_time_data_container
  =
  (
    debug "process: call";

    (* let trace_statistics, anomaly_detailed_metrics_container = *)
    (*   process_no_timestamp *)
    (*     false *)

    (*     packet_parsing_mode *)
    (*     trace_file_path *)

    (*     anomaly_container *)
    (*     filter_criteria_list *)
    (* in *)

    let trace_statistics, anomaly_detailed_metrics_container =
      if build_all_stat then
        process_parsing_all_traffic
          parallelization_mode

          packet_parsing_mode
          match_timestamps
          trace_file_path

          (* anomaly_container *)
          anomaly_slice_time_data_container          
      else
        process_parsing_anomaly_traffic
          parallelization_mode
          match_timestamps
          (* parallelization_mode *)

          packet_parsing_mode
          trace_file_path

          (* anomaly_container *)
          anomaly_slice_time_data_container
    in

    debug "process: end";

    trace_statistics, anomaly_detailed_metrics_container
  )
