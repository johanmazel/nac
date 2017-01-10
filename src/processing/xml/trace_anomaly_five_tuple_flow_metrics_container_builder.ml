
open Printf

open Map_ext_instantiations

open Traffic_flow_aggr_data_instantiations

module Five_tuple_flow_set = struct
  include Set.Make(Five_tuple_flow)

  let of_list = List.fold_left (fun acc x -> add x acc) empty
  let to_list = elements

  let to_string
      ?sep: (sep = " ")
      t
    =
    let list = elements t in

    List_ext.to_string
      ~sep
      (fun value  -> Five_tuple_flow.to_string value)
      list

end

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Trace_anomaly_five_tuple_flow_metrics_container_builder]: %s@." s)
      else
        ignore
    )
    fmt

let five_tuple_flow_count
    packet_parsing_mode
    pcap_loop
    trace_handler
  =
  (
    let set_ref = ref Five_tuple_flow_set.empty in

    Execution_time_measure.execute
      "[Trace_anomaly_five_tuple_flow_metrics_container_builder]: five_tuple_flow_count: "
      (fun _ ->
         let packet_processing_function pcap_header pcap_payload =
           Melange_wrapper.launch_function_on_header_ethernet
             (fun header ethernet_pdu ->
                let packet_data_for_metrics =
                  Packet_data_for_metrics.of_melange_ethernet
                    packet_parsing_mode
                    pcap_header
                    ethernet_pdu
                in

                let five_tuple_flow = 
                  Five_tuple_flow.of_packet_data_for_metrics
                    packet_data_for_metrics
                in

                set_ref := Five_tuple_flow_set.add five_tuple_flow !set_ref;
             )
             pcap_header
             pcap_payload
         in

         Trace_handler.launch_analysis
           pcap_loop
           trace_handler
           packet_processing_function
      );

    Five_tuple_flow_set.cardinal !set_ref
  )

let process
    parallelization_mode
    match_timestamp

    five_tuple_flow_element_anomaly_indice_container

    packet_parsing_mode
    trace_file_path
  =
  (
    debug "process: call";

    let pcap_loop = Cstruct_pcap_wrapper.launch_analysis in

    debug "process: building trace_statistics from file %s" trace_file_path;

    let trace_statistics =
      Trace_statistics.of_trace_path
        pcap_loop
        trace_file_path
    in

    let five_tuple_flow_number = 40000000 in

    (* let five_tuple_flow_anomaly_indice_container = *)
    (*   Five_tuple_flow_anomaly_indice_container.new_empty_t *)
    (*     0 *)
    (* in *)

    debug "process: building five_tuple_flow_timestamp_container";
    let five_tuple_flow_timestamp_container =
      Five_tuple_flow_timestamp_container.of_trace
        packet_parsing_mode
        match_timestamp

        trace_statistics

        trace_file_path
    in

    debug "process: building five_tuple_flow_timestamp_anomaly_indice_container";
    let five_tuple_flow_timestamp_anomaly_indice_container =
      Five_tuple_flow_timestamp_anomaly_indice_container.of_data
        parallelization_mode
        match_timestamp

        five_tuple_flow_timestamp_container
        five_tuple_flow_element_anomaly_indice_container
    in

    debug
      "process: printing five_tuple_flow_timestamp_anomaly_indice_container (length: %d)"
      (Five_tuple_flow_timestamp_anomaly_indice_container.length
         five_tuple_flow_timestamp_anomaly_indice_container
      )
    ;
    (* debug *)
    (*   "process: five_tuple_flow_timestamp_anomaly_indice_container:\n%s" *)
    (*   (Five_tuple_flow_timestamp_anomaly_indice_container.to_string *)
    (*      five_tuple_flow_timestamp_anomaly_indice_container *)
    (*   ) *)
    (* ; *)

    let anomaly_five_tuple_flow_metrics_container =
      Anomaly_five_tuple_flow_metrics_container.new_empty_t
        five_tuple_flow_number
    in

    Execution_time_measure.execute
      "[Trace_anomaly_five_tuple_flow_metrics_container_builder]: process: adding packets"
      (fun _ ->
         let packet_processing_function pcap_header pcap_payload =
           Melange_wrapper.launch_function_on_header_ethernet
             (fun pcap_header ethernet_pdu ->
                let packet_data_for_metrics =
                  Packet_data_for_metrics.of_melange_ethernet
                    packet_parsing_mode
                    pcap_header
                    ethernet_pdu
                in

                Anomaly_five_tuple_flow_metrics_container.add_packet_ethernet
                  match_timestamp

                  five_tuple_flow_timestamp_anomaly_indice_container

                  anomaly_five_tuple_flow_metrics_container

                  packet_parsing_mode
                  packet_data_for_metrics
             )
             pcap_header
             pcap_payload

         (* Melange_wrapper.launch_function_on_header_ipv4 *)
         (*   (Five_tuple_flow_metrics_container.add_packet_ipv4 *)
         (*      five_tuple_flow_metrics_container) *)
         (*   pcap_header *)
         (*   pcap_payload *)
         in

         Trace_handler.launch_analysis
           pcap_loop
           trace_statistics
           packet_processing_function
      );

    (* debug *)
    (*   "process: five_tuple_flow_detailed_metrics_container:\n%s" *)
    (*   (Five_tuple_flow_anomaly_indice_container.to_string *)
    (*      five_tuple_flow_anomaly_indice_container *)
    (*   ); *)

    (* debug *)
    (*   "process: five_tuple_flow_detailed_metrics_container:\n%s" *)
    (*   (Five_tuple_flow_element_anomaly_indice_container.to_string *)
    (*      five_tuple_flow_element_anomaly_indice_container *)
    (*   ); *)

    (* debug "process: verify timestamps in five_tuple_flow_metrics_container"; *)

    (* Five_tuple_flow_metrics_container.verify_timestamps  *)
    (*   five_tuple_flow_metrics_container; *)

    (* debug "process: verify five_tuple_flow_metrics_container"; *)

    (* Five_tuple_flow_metrics_container.verify  *)
    (*   five_tuple_flow_metrics_container; *)

    debug "process: end";

    (trace_statistics, anomaly_five_tuple_flow_metrics_container)
  )

let process_legacy
    match_timestamp

    five_tuple_flow_element_anomaly_indice_container

    packet_parsing_mode
    trace_file_path
  =
  (
    debug "process: call";

    let pcap_loop = Cstruct_pcap_wrapper.launch_analysis in

    debug "process: building trace_statistics from file %s" trace_file_path;

    let trace_statistics =
      Trace_statistics.of_trace_path
        pcap_loop
        trace_file_path
    in

    let five_tuple_flow_number = 40000000 in

    debug "process: building anomaly_five_tuple_flow_metrics_container";

    let five_tuple_flow_anomaly_indice_container =
      Five_tuple_flow_anomaly_indice_container.new_empty_t
        0
    in

    let anomaly_five_tuple_flow_metrics_container =
      Anomaly_five_tuple_flow_metrics_container.new_empty_t
        five_tuple_flow_number
    in

    Execution_time_measure.execute
      "[Trace_anomaly_five_tuple_flow_metrics_container_builder]: process: adding packets"
      (fun _ ->
         let packet_processing_function pcap_header pcap_payload =
           Melange_wrapper.launch_function_on_header_ethernet
             (fun pcap_header ethernet_pdu ->
                let packet_data_for_metrics =
                  Packet_data_for_metrics.of_melange_ethernet
                    packet_parsing_mode
                    pcap_header
                    ethernet_pdu
                in

                Anomaly_five_tuple_flow_metrics_container.add_packet_ethernet_legacy
                  match_timestamp

                  five_tuple_flow_anomaly_indice_container
                  five_tuple_flow_element_anomaly_indice_container
                  
                  anomaly_five_tuple_flow_metrics_container

                  packet_parsing_mode
                  packet_data_for_metrics
             )
             pcap_header
             pcap_payload

         (* Melange_wrapper.launch_function_on_header_ipv4 *)
         (*   (Five_tuple_flow_metrics_container.add_packet_ipv4 *)
         (*      five_tuple_flow_metrics_container) *)
         (*   pcap_header *)
         (*   pcap_payload *)
         in

         Trace_handler.launch_analysis
           pcap_loop
           trace_statistics
           packet_processing_function
      );

    (* debug *)
    (*   "process: five_tuple_flow_detailed_metrics_container:\n%s" *)
    (*   (Five_tuple_flow_anomaly_indice_container.to_string *)
    (*      five_tuple_flow_anomaly_indice_container *)
    (*   ); *)

    (* debug *)
    (*   "process: five_tuple_flow_detailed_metrics_container:\n%s" *)
    (*   (Five_tuple_flow_element_anomaly_indice_container.to_string *)
    (*      five_tuple_flow_element_anomaly_indice_container *)
    (*   ); *)

    (* debug "process: verify timestamps in five_tuple_flow_metrics_container"; *)

    (* Five_tuple_flow_metrics_container.verify_timestamps  *)
    (*   five_tuple_flow_metrics_container; *)

    (* debug "process: verify five_tuple_flow_metrics_container"; *)

    (* Five_tuple_flow_metrics_container.verify  *)
    (*   five_tuple_flow_metrics_container; *)

    debug "process: end";

    (trace_statistics, anomaly_five_tuple_flow_metrics_container)
  )
