
open Printf

module L = BatList
module HT = BatHashtbl

(* open Admd.Instantiation *)
(* open Admd_mawilab_xml_classification_instantiation *)

open Map_ext_instantiations

open Traffic_flow_aggr_data_instantiations

module Five_tuple_flow_set = Set_ext.Make(Five_tuple_flow)

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Trace_xml_data_builder]: %s@." s)
      else
        ignore
    )
    fmt

let get_trace_data_tuple
    packet_parsing_mode
    trace_file_path
    filter_criteria_list
  =
  debug "get_trace_data_tuple: call";

  let trace_statistics, five_tuple_flow_metrics_container =
    Trace_five_tuple_flow_metrics_container_builder.process
      packet_parsing_mode
      trace_file_path
  in

  debug "get_trace_data_tuple: building five_tuple_key_container";
  let five_tuple_key_container = Five_tuple_key_container.of_filter_criteria_list filter_criteria_list in

  let five_tuple_flow_metrics_container_filtered =
    Execution_time_measure.execute
      "[Trace_xml_attribute_builder]: get_trace_data_ple: filtering five_tuple_flow_metrics_container"
      (fun _ ->
         Five_tuple_flow_metrics_container.filteri
           (fun five_tuple_flow _ ->
              Five_tuple_key_container.mem_any five_tuple_flow five_tuple_key_container
           )
           five_tuple_flow_metrics_container
      )
  in

  debug 
    "get_trace_data_tuple: five_tuple_flow_metrics_container reduced from %d to %d elements"
    (Five_tuple_flow_metrics_container.length five_tuple_flow_metrics_container)
    (Five_tuple_flow_metrics_container.length five_tuple_flow_metrics_container_filtered);

  let five_tuple_key_five_tuple_flow_set_container =
    Execution_time_measure.execute
      "[Trace_xml_attribute_builder]: get_trace_data_ple: building five_tuple_key_five_tuple_flow_set_container"
      (fun _ ->
         Five_tuple_key_five_tuple_flow_set_container.of_five_tuple_flow_metrics_container
           five_tuple_flow_metrics_container_filtered
      )
  in

  debug "get_trace_data_tuple: end";

  trace_statistics,
  five_tuple_flow_metrics_container_filtered,
  five_tuple_key_five_tuple_flow_set_container

