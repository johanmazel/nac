
open Printf

module L = BatList
module HT = BatHashtbl

open Map_ext_instantiations

open Traffic_flow_aggr_data_instantiations

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Trace_classifier]: %s@." s)
      else
        ignore
    )
    fmt

let process
    parallelization_mode
    packet_parsing_mode
    check_five_tuple_flow_metrics_timestamp
    
    export_metrics_attributes

    taxonomy_filepath

    trace_file_path
  =
  (
    debug "process: call";

    let trace_file_name = Filename.basename trace_file_path in

    let indice_name_tuple_array = 
      Network_traffic_attributes.to_indice_name_tuple_array
        ()
    in
    let feature_array = Array.map (fun (indice , name) -> Feature.new_t indice name) indice_name_tuple_array in
    let global_feature_container = Feature_container.new_t feature_array in

    let indice_name_tuple_array = Detailed_metrics.to_indice_name_tuple_array () in
    let metric_array = Array.map (fun (indice , name) -> Feature.new_t indice name) indice_name_tuple_array in
    let global_metric_container = Feature_container.new_t metric_array in

    let anomaly_taxonomy = 
      Anomaly_taxonomy.of_string_tuple_ptree
        global_feature_container
        global_metric_container

        taxonomy_filepath
    in

    debug "process: verifying taxonomy";

    if Taxonomy_verifier.verify_taxonomy anomaly_taxonomy = false then
      (
        print_endline "Xml_classifier: process: invalid taxonomy";
        assert(false);
      );

    let pcap_loop = Cstruct_pcap_wrapper.launch_analysis in

    let trace_statistics =
      Trace_statistics.of_trace_path
        pcap_loop
        trace_file_path
    in

    debug "process: building five_tuple_flow_metrics_container";

    let five_tuple_flow_metrics_container =
      Five_tuple_flow_metrics_container.new_empty_t
        40000000
    in

    Execution_time_measure.execute
      "[Trace_classifier]: process: adding packets"
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

                Five_tuple_flow_metrics_container.add_packet_ethernet
                  five_tuple_flow_metrics_container
                  packet_parsing_mode
                  packet_data_for_metrics
             )
             pcap_header
             pcap_payload
         in

         Trace_handler.launch_analysis
           pcap_loop
           trace_statistics
           packet_processing_function
      );

    (* debug *)
    (*   "process: five_tuple_flow_detailed_metrics_container:\n%s" *)
    (*   (Five_tuple_flow_metrics_container.to_string *)
    (*      To_string_mode.Simple *)
    (*      five_tuple_flow_metrics_container); *)

    Five_tuple_flow_metrics_container.verify_timestamps 
      five_tuple_flow_metrics_container;

    debug
      "process: five_tuple_flow_metrics_container length: %d"
      (Five_tuple_flow_metrics_container.length five_tuple_flow_metrics_container);

    Execution_time_measure.execute
      "[Trace_classifier]: process: GCing"
      (fun _ ->
         Gc.full_major ()
      );

    (* debug *)
    (*   "process: traffic_flow_metrics_container:\n%s" *)
    (*   (Five_tuple_flow_metrics_container.to_string *)
    (*      To_string_mode.Normal *)
    (*      five_tuple_flow_metrics_container *)
    (*   ) *)
    (* ; *)

    debug "process: building five_tuple_flow_detailed_metrics_container";

    let five_tuple_flow_detailed_metrics_container =
      Traffic_flow_detailed_metrics_aggr_data.Simple_key_data_container.of_simple_key_data_hashtable
        (
          Five_tuple_flow_metrics_container.map_to_hashtbl
            (fun five_tuple_flow five_tuple_flow_metrics ->
               Detailed_metrics.of_five_tuple_flow_metrics
                 ~check_five_tuple_flow_metrics_timestamp: check_five_tuple_flow_metrics_timestamp
                 five_tuple_flow 
                 five_tuple_flow_metrics
            )
            five_tuple_flow_metrics_container
        )
    in

    (* debug *)
    (*   "process: five_tuple_flow_detailed_metrics_container:\n%s" *)
    (*   (Traffic_flow_detailed_metrics_aggr_data.Simple_key_data_container.to_string *)
    (*      To_string_mode.Normal *)
    (*      five_tuple_flow_detailed_metrics_container *)
    (*   ) *)
    (* ; *)

    let traffic_flow_detailed_metrics_aggregated_container =
      Execution_time_measure.execute
        "[Trace_classifier]: process: aggregating five_tuple_flow detailed_metrics"
        (fun _ ->
           Traffic_flow_detailed_metrics_aggregated_container.of_addr_mode_list_five_tuple_flow_detailed_metrics_container
             [ Traffic_flow_key_type.Src_addr_mode; Traffic_flow_key_type.Dst_addr_mode ]
             five_tuple_flow_detailed_metrics_container
        )
    in

    (* debug *)
    (*   "process: traffic_flow_detailed_metrics_aggregated_container:\n%s" *)
    (*   (Traffic_flow_detailed_metrics_aggregated_container.to_string *)
    (*      traffic_flow_detailed_metrics_aggregated_container *)
    (*   ) *)
    (* ; *)

    (* debug "process: building classification data"; *)

    let traffic_flow_classification_data_aggregated_container =
      Execution_time_measure.execute
        "[Trace_classifier]: process: building classification data"
        (fun _ ->
           Traffic_flow_classification_data_aggregated_container.of_traffic_flow_detailed_metrics_aggregated_container
             trace_statistics
             traffic_flow_detailed_metrics_aggregated_container
        )
    in

    debug "process: classifying %d elements" 
      (Traffic_flow_classification_data_aggregated_container.length_total traffic_flow_classification_data_aggregated_container)
    ;

    let traffic_flow_anomaly_data_signature_aggregated_container =
      Execution_time_measure.execute
        "[Trace_classifier]: process: classifying"
        (fun _ ->
           Traffic_flow_anomaly_data_signature_aggregated_container.of_traffic_flow_classification_data_aggregated_container
             parallelization_mode
             anomaly_taxonomy
             traffic_flow_classification_data_aggregated_container
        )
    in

    debug "process: building admd data";

    let key_aggr_anomaly_data_signature_tuple_list =
      Traffic_flow_anomaly_data_signature_aggregated_container.fold
        (fun key_aggr_mode traffic_flow_anomaly_data_signature_aggr global_list ->
           (* Traffic_flow_anomaly_data_signature_aggr_data.Aggr_key_data_container.fold *)
           HT.fold
             (fun key_aggr anomaly_data_signature list ->
                (key_aggr, anomaly_data_signature) :: list
             )
             traffic_flow_anomaly_data_signature_aggr
             global_list
        )
        traffic_flow_anomaly_data_signature_aggregated_container
        []
    in


    (* let admd_anomaly_list = *)
    (*   L.mapi *)
    (*     (fun indice (key_aggr, anomaly_data_signature) -> *)
    (*        ( *)
    (*          let detailed_metrics  =  *)
    (*            anomaly_data_signature.Anomaly_data_signature.detailed_metrics *)
    (*          in *)
    (*          let network_traffic_attributes  =  *)
    (*            anomaly_data_signature.Anomaly_data_signature.network_traffic_attributes *)
    (*          in *)
    (*          let network_traffic_values  =  *)
    (*            anomaly_data_signature.Anomaly_data_signature.network_traffic_values *)
    (*          in *)

    (*          let anomaly_type_string = *)
    (*            Anomaly_signature.to_string *)
    (*              To_string_mode.Command *)
    (*              anomaly_data_signature.Anomaly_data_signature.anomaly_signature *)
    (*          in *)

    (*          let description_string = *)
    (*            (Detailed_metrics.to_string  *)
    (*               To_string_mode.Simple *)
    (*               anomaly_data_signature.Anomaly_data_signature.detailed_metrics *)
    (*            ) *)
    (*            ^ "\n--\n" ^ *)
    (*            (Network_traffic_attributes.to_string *)
    (*               To_string_mode.Normal *)
    (*               network_traffic_attributes) *)
    (*            ^ "\n--\n" ^ *)
    (*            (Network_traffic_values.to_string *)
    (*               To_string_mode.Normal *)
    (*               network_traffic_values) *)
    (*            ^ "\n--\n" ^ *)
    (*            (Sexplib.Sexp.to_string *)
    (*               (Network_traffic_attributes.sexp_of_t *)
    (*                  network_traffic_attributes) *)
    (*            ) *)
    (*            ^ "\n--\n" ^ *)
    (*            (Sexplib.Sexp.to_string *)
    (*               (Network_traffic_values.sexp_of_t *)
    (*                  network_traffic_values) *)
    (*            ) *)
    (*          in *)

    (*          let filter_criteria =  *)
    (*            match key_aggr with *)
    (*            | Traffic_flow_key_type.All -> failwith "Trace_classifier: trying to export anomaly matched on all traffic => bad idea" *)
    (*            | Traffic_flow_key_type.Src_addr addr -> *)
    (*               Admd.Filter.criteria.Src_ip (Admd_ipaddr.of_ipaddr addr) *)
    (*            |Traffic_flow_key_type.Dst_addr addr -> *)
    (*               Admd.Filter.criteria.Dst_ip (Admd_ipaddr.of_ipaddr addr) *)
    (*          in *)
    (*          let filter =  Admd.Filter.new_t [ filter_criteria ] in *)
    (*          let slice = Admd_slice.new_t [ filter ] (Some 0) (Some 0) in *)

    (*          let anomaly = *)
    (*            Base.Anomaly.new_t *)
    (*              indice *)

    (*              date *)
    (*              time *)

    (*              anomaly_type_string *)
    (*              "" *)
    (*              (match export_metrics_attributes with *)
    (*               | false -> Some "" *)
    (*               | true -> Some description_string) *)
    (*              [ slice ] *)

    (*              detailed_metrics.Detailed_metrics.timestamp_sec_start *)
    (*              (detailed_metrics.Detailed_metrics.timestamp_sec_end + 1) *)
    (*          in *)

    (*          anomaly *)
    (*        ) *)
    (*     ) *)
    (*     key_aggr_anomaly_data_signature_tuple_list *)
    (* in *)

    (* let anomaly_container = *)
    (*   Base.Anomaly_container.new_t *)
    (*     admd_anomaly_list *)
    (* in *)

    let attributes_metadata_string =
      Sexplib.Sexp.to_string
        (Feature_name_container.sexp_of_t
           (Network_traffic_attributes.generate_feature_name_container
              ()
           )
        )
    in

    let algorithm = Generate_admd_data.algorithm in
    let analysis = Generate_admd_data.analysis attributes_metadata_string in
    let dataset = Generate_admd_data.dataset in

    let algorithm_option = Some algorithm in
    let analysis_option =
      match export_metrics_attributes with
      | false -> None
      | true -> Some analysis
    in
    let dataset_option = Some dataset in

    (* let basename_wo_extension = Filename.chop_extension (Filename.basename trace_file_name) in *)

    (* debug *)
    (*   "export_xml: trace_file_name: %s ; basename_wo_extension: %s" *)
    (*   trace_file_name *)
    (*   basename_wo_extension *)
    (* ; *)
    debug
      "export_xml: trace_file_name: %s"
      trace_file_name
    ;

    (* let anomaly_file_filename = (Filename.chop_extension basename) ^ "_cl.xml" in *)

    (* let anomaly_file = *)
    (*   Base.File.new_t *)
    (*     anomaly_file_filename *)

    (*     date *)
    (*     time *)

    (*     (Some algorithm) *)
    (*     (match export_metrics_attributes with *)
    (*      | false -> None *)
    (*      | true -> Some analysis *)
    (*     ) *)
    (*     (Some dataset) *)

    (*     anomaly_container *)
    (* in *)

    (* Base.File.to_filename *)
    (*   (match export_metrics_attributes with *)
    (*    | false -> To_string_mode.Command *)
    (*    | true -> To_string_mode.Simple *)
    (*   ) *)
    (*   anomaly_file;     *)


    let feature_name_container =
      Feature_name_container.of_indice_name_tuple_array
        (Network_traffic_attributes.to_indice_name_tuple_array
           ()
        )
    in


    let anomaly_data_l =
      L.mapi
        (fun indice (key_aggr, anomaly_data_signature) ->
           let anomaly_type = Mawilab_anomaly_type.Anomalous in

           let anomaly_signature =
             anomaly_data_signature.Anomaly_data_signature.anomaly_signature
           in

           let base_value = "none" in

           let initial_description_string = "" in

           let detailed_metrics  = 
             anomaly_data_signature.Anomaly_data_signature.detailed_metrics
           in
           let detailed_metrics_string = 
             Detailed_metrics.to_string
               detailed_metrics
           in

           let network_traffic_attributes  = 
             anomaly_data_signature.Anomaly_data_signature.network_traffic_attributes
           in

           let flow = 0. in
           let nb_packets =
             Network_traffic_attributes.find_name
               feature_name_container
               network_traffic_attributes
               "nb_packets"
           in
           let nb_bytes =
             Network_traffic_attributes.find_name
               feature_name_container
               network_traffic_attributes
               "nb_bytes"
           in
           let anomaly_metric =
             Anomaly_metric.new_t
               flow
               nb_packets
               nb_bytes
           in

           let network_traffic_values  = 
             anomaly_data_signature.Anomaly_data_signature.network_traffic_values
           in

           let anomaly_raw_data  = 
             anomaly_data_signature.Anomaly_data_signature.anomaly_raw_data
           in

           let filter_criteria = 
             match key_aggr with
             | Traffic_flow_key_type.All -> failwith "Trace_classifier: trying to export anomaly matched on all traffic => bad idea"
             | Traffic_flow_key_type.Src_addr addr ->
               Admd.Filter_criteria.Src_ip ( Admd.Ipaddr_sb.of_ipaddr addr)
             |Traffic_flow_key_type.Dst_addr addr ->
               Admd.Filter_criteria.Dst_ip ( Admd.Ipaddr_sb.of_ipaddr addr)
           in
           let filter =  Admd.Filter.new_t [ filter_criteria ] in
           let slice =  Admd.Slice.new_t [ filter ] (Some 0) (Some 0) in
           let slice_list = [ slice ] in

           let start_sec = detailed_metrics.Detailed_metrics.timestamp_sec_start in
           let stop_sec = (detailed_metrics.Detailed_metrics.timestamp_sec_end + 1) in

           Anomaly_data.new_t
             (* date *)
             (* time *)

             anomaly_type
             base_value

             initial_description_string
             detailed_metrics_string

             anomaly_metric
             network_traffic_attributes
             network_traffic_values
             anomaly_raw_data

             anomaly_signature

             slice_list

             start_sec
             0
             stop_sec
             0
        )
        key_aggr_anomaly_data_signature_tuple_list
    in

    let anomaly_data_container =
      Anomaly_data_container.of_data
        trace_file_path

        (* date *)
        (* time *)

        algorithm_option
        analysis_option
        dataset_option

        anomaly_data_l
    in

    Anomaly_data_container.export_xml
      export_metrics_attributes
      anomaly_data_container
    ;
    Anomaly_data_container.export_binary      
      anomaly_data_container
    ;

    (* let data_for_xml_l = *)
    (*   L.map *)
    (*     (fun *)
    (*       ( *)
    (*         indice, *)
    (*         date, *)
    (*         time, *)

    (*         anomaly_type, *)
    (*         base_value, *)

    (*         initial_description_string, *)
    (*         detailed_metrics_string, *)
    (*         anomaly_metric, *)
    (*         network_traffic_attributes, *)
    (*         network_traffic_values, *)
    (*         anomaly_raw_data, *)

    (*         slice_list, *)

    (*         start_time, *)
    (*         end_time *)
    (*       ) *)
    (*       -> *)
    (*         indice, *)
    (*         date, *)
    (*         time, *)

    (*         anomaly_type, *)
    (*         base_value, *)

    (*         initial_description_string, *)
    (*         detailed_metrics_string, *)
    (*         anomaly_metric, *)
    (*         network_traffic_attributes, *)
    (*         network_traffic_values, *)

    (*         slice_list, *)

    (*         start_time, *)
    (*         end_time *)
    (*     ) *)
    (*     data_l *)
    (* in *)

    (* Export_mawilab_xml_admd.process *)
    (*   basename *)

    (*   date *)
    (*   time *)

    (*   algorithm_option *)
    (*   analysis_option *)
    (*   dataset_option *)

    (*   export_metrics_attributes *)

    (*   data_for_xml_l *)
    (* ; *)

    (* let basename = Filename.basename trace_file_path in *)


    (* debug "process: export binary"; *)

    (* let data_for_binary_l = *)
    (*   L.map *)
    (*     (fun *)
    (*       ( *)
    (*         indice, *)
    (*         date, *)
    (*         time, *)

    (*         anomaly_type, *)
    (*         base_value, *)

    (*         initial_description_string, *)
    (*         detailed_metrics_string, *)
    (*         anomaly_metric, *)
    (*         network_traffic_attributes, *)
    (*         network_traffic_values, *)
    (*         anomaly_raw_data, *)

    (*         slice_list, *)

    (*         start_time, *)
    (*         end_time *)
    (*       ) *)
    (*       -> *)
    (*         indice, *)
    (*         date, *)
    (*         time, *)

    (*         anomaly_type, *)
    (*         base_value, *)

    (*         (\* initial_description_string, *\) *)
    (*         (\* detailed_metrics_string, *\) *)
    (*         anomaly_metric, *)
    (*         network_traffic_attributes, *)
    (*         network_traffic_values, *)
    (*         anomaly_raw_data, *)

    (*         slice_list, *)

    (*         start_time, *)
    (*         end_time *)
    (*     ) *)
    (*     data_l *)
    (* in *)

    (* Export_binary_day_anomaly_container.process *)
    (*   basename *)
    (*   time *)
    (*   data_for_binary_l *)
    (* ; *)

    debug "process: end";
  )
