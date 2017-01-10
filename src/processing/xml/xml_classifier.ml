
open Printf

module A = BatArray
module S = BatString
module HT = BatHashtbl
module L = BatList

open Admd.Instantiation

open Set_ext_instantiations
open Map_ext_instantiations

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Xml_classifier]: %s@." s)
      else
        ignore
    )
    fmt

let map_to_anomaly_data    
    intial_description_h
    detailed_metrics_string_h

    anomaly_detailed_metrics_container
    anomaly_network_traffic_attributes_values_container

    anomaly_raw_data_int_map
    anomaly_signature_int_map

    anomaly
  =
  let anomaly_type = Mawilab_anomaly_type.of_string anomaly.Admd.Instantiation.Base.Anomaly.anomaly_type in
  let base_value = anomaly.Admd.Instantiation.Base.Anomaly.anomaly_value in

  let indice = anomaly.Admd.Instantiation.Base.Anomaly.indice in

  let initial_description_string =
    HT.find
      intial_description_h
      indice
  in

  let detailed_metrics_string = 
    HT.find
      detailed_metrics_string_h
      indice
  in

  let anomaly_metric =
    Anomaly_detailed_metrics_container.find_anomaly_metric
      indice
      anomaly_detailed_metrics_container
  in
  let network_traffic_attributes =
    fst
      (Anomaly_network_traffic_attributes_values_container.find
         indice
         anomaly_network_traffic_attributes_values_container
      )
  in
  let network_traffic_values =
    snd
      (Anomaly_network_traffic_attributes_values_container.find
         indice
         anomaly_network_traffic_attributes_values_container
      )
  in

  let anomaly_raw_data =
    Int_map.find
      indice
      anomaly_raw_data_int_map
  in

  let anomaly_signature =
    Int_map.find
      indice
      anomaly_signature_int_map
  in

  let slice_list = anomaly.Admd.Instantiation.Base.Anomaly.slice_list in

  let start_sec = anomaly.Admd.Instantiation.Base.Anomaly.start_sec in
  let start_usec = anomaly.Admd.Instantiation.Base.Anomaly.start_usec in
  let stop_sec = anomaly.Admd.Instantiation.Base.Anomaly.stop_sec in
  let stop_usec = anomaly.Admd.Instantiation.Base.Anomaly.stop_usec in

  Anomaly_data.new_t
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
    start_usec
    stop_sec
    stop_usec
        



let process
    parallelization_mode

    packet_parsing_mode
    match_timestamps
    build_all_stat
    check_five_tuple_flow_metrics_timestamp
    export_metrics_attributes

    taxonomy_filepath

    xml_attribute_classification_mode
  =
  (
    debug "process: call";

    let indice_name_tuple_array = Network_traffic_attributes.to_indice_name_tuple_array () in
    let feature_array = Array.map (fun (indice , name) -> Feature.new_t indice name) indice_name_tuple_array in
    let attribute_feature_container = Feature_container.new_t feature_array in

    let indice_name_tuple_array = Detailed_metrics.to_indice_name_tuple_array () in
    let feature_array = Array.map (fun (indice , name) -> Feature.new_t indice name) indice_name_tuple_array in
    let value_feature_container = Feature_container.new_t feature_array in

    debug "process: reading taxonomy";
    let anomaly_taxonomy = 
      Anomaly_taxonomy.of_string_tuple_ptree
        attribute_feature_container
        value_feature_container
        taxonomy_filepath
    in

    debug "process: verifying taxonomy";
    if Taxonomy_verifier.verify_taxonomy anomaly_taxonomy = false then
      (
        print_endline "Xml_classifier: process: invalid taxonomy";
        assert(false);
      );

    ignore(
      match xml_attribute_classification_mode with
      | Xml_attribute_building_mode.Xml xml_file_path ->
        (
          print_endline "Xml_classifier: process: extracting from xml";

          (* let xml_file_basename_no_extension, *)
          (*     mawilab_admd_file, *)
          (*     mawilab_mod_anomaly_description_h *)
          (*   = *)
          (*   Xml_attribute_builder.process *)
          (*     parallelization_mode *)

          (*     date_format_string *)
          (*     ?default_hour_minute_second *)
          (*     time_format_string *)

          (*     (\* detector_container_file_name *\) *)
          (*     (\* detector_setting_container_file_name *\) *)

          (*     xml_file_path *)
          (* in *)

          (* let _mawilab_mod_admd_file = *)
          (*   Xml_file_classifier.process_and_export *)
          (*     anomaly_taxonomy *)

          (*     export_metrics_attributes *)

          (*     xml_file_basename_no_extension *)
          (*     mawilab_admd_file *)
          (*     mawilab_mod_anomaly_description_h *)
          (* in *)

          (* let *)
          (*   anomaly_signature_int_map *)
          (*   = *)
          (*   Xml_file_classifier.process *)
          (*     anomaly_taxonomy *)

          (*     export_metrics_attributes *)

          (*     detailed_metrics_string_h *)
          (*     anomaly_network_attributes_values_container *)

          (*     mawilab_admd_file *)
          (* in *)

          assert(false);
        )
      | Xml_attribute_building_mode.Trace_xml (trace_file_path, xml_file_path) ->
        (
          print_endline "[Xml_classifier]: process: extracting from trace and xml";

          debug "process: building admd_file from file %s" xml_file_path;

          let
            mawilab_admd_file,
            anomaly_container
            =
            Admd_xml_file_reader.process
              parallelization_mode

              xml_file_path
          in

          (* let filter_criteria_list = *)
          (*   Filter_criteria_extractor.of_anomaly_container *)
          (*     anomaly_container *)
          (* in *)

          let anomaly_slice_time_data_container =
            Anomaly_slice_time_data_container.of_anomaly_container
              anomaly_container
          in

          let (trace_statistics : Trace_statistics.t), anomaly_detailed_metrics_container =
            Anomaly_detailed_metrics_builder.process
              parallelization_mode

              packet_parsing_mode
              match_timestamps
              build_all_stat              
              check_five_tuple_flow_metrics_timestamp
              
              trace_file_path

              (* filter_criteria_list *)
              (* anomaly_container *)
              anomaly_slice_time_data_container
          in

          debug
            "process: anomaly_detailed_metrics_container length: %d"
            (Anomaly_detailed_metrics_container.length
               anomaly_detailed_metrics_container
            )
          ;

          let
            initial_description_string_int_map,
            detailed_metrics_string_h,

            anomaly_network_attributes_values_container,
            anomaly_raw_data_int_map

            ,
            mawilab_description_for_xml_classification_h
            =
            Data_hashtable_builder.process
              trace_statistics
              anomaly_detailed_metrics_container 

              anomaly_container
          in          



          let
            anomaly_signature_int_map
            =
            Xml_file_classifier.process
              anomaly_taxonomy

              export_metrics_attributes

              detailed_metrics_string_h
              anomaly_network_attributes_values_container

              mawilab_admd_file
          in

          let anomaly_container =
            mawilab_admd_file.Admd.Instantiation.Base.File.anomaly_container
          in

          let anomaly_data_l : Anomaly_data.t list =
            L.map
              (map_to_anomaly_data
                 initial_description_string_int_map
                 detailed_metrics_string_h

                 anomaly_detailed_metrics_container
                 anomaly_network_attributes_values_container

                 anomaly_raw_data_int_map
                 anomaly_signature_int_map
              )
              anomaly_container.Admd.Instantiation.Base.Anomaly_container.anomaly_list
          in

          let algorithm_option = mawilab_admd_file.Admd.Instantiation.Base.File.algorithm_option in
          let analysis_option = mawilab_admd_file.Admd.Instantiation.Base.File.analysis_option in
          let dataset_option = mawilab_admd_file.Admd.Instantiation.Base.File.dataset_option in

          let anomaly_data_container =
            Anomaly_data_container.of_data
              xml_file_path

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
        )
      | Xml_attribute_building_mode.Trace_mawilab_xml
          (
            trace_file_path, 
            anomalous_suspicious_xml_file_path,
            notice_xml_file_path
          )
        ->
        (
          (* TODO: ask processing of timestamps *)
          print_endline "Xml_classifier: process: extracting from trace and xml";

          let
            anomalous_suspicious_mawilab_admd_file,
            anomalous_suspicious_anomaly_container
            =
            Admd_xml_file_reader.process
              parallelization_mode

              anomalous_suspicious_xml_file_path
          in
          let
            notice_mawilab_admd_file,
            notice_anomaly_container
            =
            Admd_xml_file_reader.process

              parallelization_mode

              notice_xml_file_path
          in

          (* let anomalous_suspicious_filter_criteria_list = *)
          (*   Filter_criteria_extractor.of_anomaly_container *)
          (*     anomalous_suspicious_anomaly_container *)
          (* in *)
          (* let notice_filter_criteria_list = *)
          (*   Filter_criteria_extractor.of_anomaly_container *)
          (*     notice_anomaly_container *)
          (* in *)
          (* let filter_criteria_list = *)
          (*   L.sort_unique  *)
          (*     Admd.Filter_criteria.compare  *)
          (*     (L.append anomalous_suspicious_filter_criteria_list notice_filter_criteria_list) *)
          (* in *)

          (* print_endline "Xml_classifier: process: reading trace"; *)
          (* let *)
          (*   trace_statistics, *)
          (*   five_tuple_flow_metrics_container, *)
          (*   five_tuple_key_five_tuple_flow_set_container *)
          (*   = *)
          (*   Trace_xml_data_builder.get_trace_data_tuple *)
          (*     packet_parsing_mode *)
          (*     trace_file_path *)
          (*     filter_criteria_list *)
          (* in *)

          (* print_endline "Xml_classifier: process: building anomaly_detailed_metrics_container"; *)
          (* let anomalous_suspicious_anomaly_detailed_metrics_container = *)
          (*   Anomaly_detailed_metrics_container.of_anomaly_container_five_tuple_flow_metrics_container *)
          (*     parallelization_mode *)

          (*     five_tuple_flow_metrics_container *)
          (*     five_tuple_key_five_tuple_flow_set_container *)

          (*     anomalous_suspicious_anomaly_container *)
          (* in *)
          (* let notice_anomaly_detailed_metrics_container = *)
          (*   Anomaly_detailed_metrics_container.of_anomaly_container_five_tuple_flow_metrics_container *)
          (*     parallelization_mode *)

          (*     five_tuple_flow_metrics_container *)
          (*     five_tuple_key_five_tuple_flow_set_container *)

          (*     notice_anomaly_container *)
          (* in *)

          let anomalous_suspicious_anomaly_container_length =
            Base.Anomaly_container.length
              anomalous_suspicious_anomaly_container
          in
          let anomaly_slice_time_data_container =
            Anomaly_slice_time_data_container.of_double_anomaly_container
              anomalous_suspicious_anomaly_container
              notice_anomaly_container
          in

          (* let anomalous_suspicious_indice_int_set = *)
          (*   Int_set.of_list *)
          (*     (L.map *)
          (*        (fun anomaly -> *)
          (*           anomaly.Base.Anomaly.indice *)
          (*        ) *)
          (*        anomalous_suspicious_anomaly_container.Base.Anomaly_container.anomaly_list *)
          (*     ) *)
          (* in *)
          (* let notice_indice_int_set = *)
          (*   Int_set.of_list *)
          (*     (L.map *)
          (*        (fun anomaly -> *)
          (*           anomaly.Base.Anomaly.indice *)
          (*        ) *)
          (*        notice_anomaly_container.Base.Anomaly_container.anomaly_list *)
          (*     ) *)
          (* in *)

          let (trace_statistics : Trace_statistics.t), anomaly_detailed_metrics_container =
            Anomaly_detailed_metrics_builder.process
              parallelization_mode

              packet_parsing_mode
              match_timestamps
              build_all_stat
              check_five_tuple_flow_metrics_timestamp
              
              trace_file_path

              (* filter_criteria_list *)
              (* () *)
              (* anomaly_container *)
              anomaly_slice_time_data_container
          in

          let data_l =
            Anomaly_detailed_metrics_container.to_list
              anomaly_detailed_metrics_container
          in
          let data_l_sorted =
            L.sort
              (fun (i1, _) (i2, _) -> compare i1 i2)
              data_l
          in

          let anomalous_suspicious_l, notice_l =
            L.split_at
              anomalous_suspicious_anomaly_container_length
              data_l_sorted
          in

          let anomalous_suspicious_l_admd_i =
            L.map
              (fun (indice, data) ->
                 let admd_indice : int =
                   Anomaly_slice_time_data_container.get_admd_indice
                     anomaly_slice_time_data_container
                     indice
                 in
                 admd_indice, data
              )
              anomalous_suspicious_l
          in
          let notice_l_admd_i =
            L.map
              (fun (indice, data) ->
                 let admd_indice =
                   Anomaly_slice_time_data_container.get_admd_indice
                     anomaly_slice_time_data_container
                     indice
                 in
                 admd_indice, data
              )
              notice_l
          in

          let anomalous_suspicious_anomaly_detailed_metrics_container =
            Anomaly_detailed_metrics_container.of_list
              anomalous_suspicious_l_admd_i
          in
          let notice_anomaly_detailed_metrics_container =
            Anomaly_detailed_metrics_container.of_list
              notice_l_admd_i
          in



          let anomalous_suspicious_detailed_metrics_string_h =
            HT.map
              (fun indice detailed_metrics ->
                 Detailed_metrics.to_string
                   detailed_metrics
              )
              anomalous_suspicious_anomaly_detailed_metrics_container.Anomaly_detailed_metrics_container.detailed_metrics_h
          in
          let notice_detailed_metrics_string_h =
            HT.map
              (fun indice detailed_metrics ->
                 Detailed_metrics.to_string
                   detailed_metrics
              )
              notice_anomaly_detailed_metrics_container.Anomaly_detailed_metrics_container.detailed_metrics_h
          in


          print_endline "Xml_classifier: process: building int_map";
          let
            anomalous_suspicious_initial_description_string_int_map,
            anomalous_suspicious_detailed_metrics_string_int_map,

            anomalous_suspicious_anomaly_network_traffic_attributes_values_container,
            anomalous_suspicious_anomaly_raw_data_int_map

            ,
            anomalous_suspicious_mawilab_description_for_xml_classification_int_map
            =
            Data_hashtable_builder.process
              trace_statistics
              anomalous_suspicious_anomaly_detailed_metrics_container 

              anomalous_suspicious_anomaly_container
          in
          let
            notice_initial_description_string_int_map,
            notice_detailed_metrics_string_int_map,

            notice_anomaly_network_traffic_attributes_values_container,
            notice_anomaly_raw_data_int_map

            ,
            notice_mawilab_description_for_xml_classification_h
            =
            Data_hashtable_builder.process
              trace_statistics
              notice_anomaly_detailed_metrics_container 

              notice_anomaly_container
          in




          (* let *)
          (*   _anomalous_suspicious_anomaly_signature_int_map *)
          (*   = *)
          (*   Xml_file_classifier.process_and_export *)
          (*     anomaly_taxonomy *)

          (*     export_metrics_attributes *)

          (*     anomalous_suspicious_xml_file_basename_no_extension *)
          (*     anomalous_suspicious_mawilab_admd_file *)
          (*     anomalous_suspicious_mawilab_description_for_xml_classification_int_map *)
          (* in *)
          (* let *)
          (*   _notice_anomaly_signature_int_map *)
          (*   = *)
          (*   Xml_file_classifier.process_and_export *)
          (*     anomaly_taxonomy *)

          (*     export_metrics_attributes *)

          (*     notice_xml_file_basename_no_extension *)
          (*     notice_mawilab_admd_file *)
          (*     notice_mawilab_description_for_xml_classification_h *)
          (* in *)





          let
            anomalous_suspicious_anomaly_signature_int_map
            =
            Xml_file_classifier.process
              anomaly_taxonomy

              export_metrics_attributes

              anomalous_suspicious_detailed_metrics_string_h
              anomalous_suspicious_anomaly_network_traffic_attributes_values_container

              anomalous_suspicious_mawilab_admd_file
          in
          let
            notice_anomaly_signature_int_map
            =
            Xml_file_classifier.process
              anomaly_taxonomy

              export_metrics_attributes

              notice_detailed_metrics_string_h
              notice_anomaly_network_traffic_attributes_values_container

              notice_mawilab_admd_file
          in


          let anomalous_suspicious_anomaly_container =
            anomalous_suspicious_mawilab_admd_file.Admd.Instantiation.Base.File.anomaly_container
          in
          let notice_anomaly_container =
            notice_mawilab_admd_file.Admd.Instantiation.Base.File.anomaly_container
          in

          let anomalous_suspicious_anomaly_data_l =
            L.map
              (map_to_anomaly_data
                 (* date *)
                 (* time *)

                 anomalous_suspicious_initial_description_string_int_map
                 anomalous_suspicious_detailed_metrics_string_h

                 anomalous_suspicious_anomaly_detailed_metrics_container
                 anomalous_suspicious_anomaly_network_traffic_attributes_values_container

                 anomalous_suspicious_anomaly_raw_data_int_map
                 anomalous_suspicious_anomaly_signature_int_map
              )
              anomalous_suspicious_anomaly_container.Admd.Instantiation.Base.Anomaly_container.anomaly_list
          in

          let notice_anomaly_data_l =
            L.map
              (map_to_anomaly_data
                 (* date *)
                 (* time *)

                 notice_initial_description_string_int_map
                 notice_detailed_metrics_string_h

                 notice_anomaly_detailed_metrics_container
                 notice_anomaly_network_traffic_attributes_values_container

                 notice_anomaly_raw_data_int_map
                 notice_anomaly_signature_int_map
              )
              notice_anomaly_container.Admd.Instantiation.Base.Anomaly_container.anomaly_list
          in

          (* let date = anomalous_suspicious_mawilab_admd_file.Admd.Instantiation.Base.File.date in *)
          (* let time = anomalous_suspicious_mawilab_admd_file.Admd.Instantiation.Base.File.time in *)

          let algorithm_option = anomalous_suspicious_mawilab_admd_file.Admd.Instantiation.Base.File.algorithm_option in
          let analysis_option = anomalous_suspicious_mawilab_admd_file.Admd.Instantiation.Base.File.analysis_option in
          let dataset_option = anomalous_suspicious_mawilab_admd_file.Admd.Instantiation.Base.File.dataset_option in

          let anomalous_suspicious_anomaly_data_container =
            Anomaly_data_container.of_data
              anomalous_suspicious_xml_file_path

              (* date *)
              (* time *)

              algorithm_option
              analysis_option
              dataset_option

              anomalous_suspicious_anomaly_data_l
          in
          Anomaly_data_container.export_xml
            export_metrics_attributes
            anomalous_suspicious_anomaly_data_container
          ;

          let notice_anomaly_data_container =
            Anomaly_data_container.of_data
              notice_xml_file_path

              (* date *)
              (* time *)

              algorithm_option
              analysis_option
              dataset_option

              notice_anomaly_data_l
          in
          Anomaly_data_container.export_xml
            export_metrics_attributes
            notice_anomaly_data_container
          ;

          let anomaly_data_l =
            (L.append
               anomalous_suspicious_anomaly_data_l
               notice_anomaly_data_l
            )
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

          Anomaly_data_container.export_binary      
            anomaly_data_container
          ;
        )
    );

    debug "process: end";
  )
