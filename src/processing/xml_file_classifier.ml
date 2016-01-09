
open Printf

module HT = BatHashtbl
  
open Admd.Instantiation

open Map_ext_instantiations

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Xml_file_classifier]: %s@." s)
      else
        ignore
    )
    fmt

(* let process_and_export *)
(*     anomaly_taxonomy *)

(*     export_metrics_attributes *)

(*     xml_file_basename_no_extension *)
(*     mawilab_admd_file *)
(*     mawilab_description_for_xml_classification_h *)
(*   = *)
(*   ( *)
(*     debug "process: classification"; *)

(*     let anomaly_container = mawilab_admd_file.Base.File.anomaly_container in *)

(*     let anomaly_list = anomaly_container.Base.Anomaly_container.anomaly_list in *)

(*     let anomaly_int_map = *)
(*       List.fold_left *)
(*         (fun int_map anomaly -> *)
(*            ( *)
(*              Int_map.add *)
(*                anomaly.Base.Anomaly.indice *)
(*                anomaly *)
(*                int_map *)
(*            ) *)
(*         ) *)
(*         Int_map.empty *)
(*         anomaly_list *)
(*     in *)

(*     let anomaly_taxonomy_manager = *)
(*       Anomaly_taxonomy_manager.of_anomaly_taxonomy *)
(*         anomaly_taxonomy *)
(*     in *)

(*     let ple_int_map = *)
(*       Execution_time_measure.execute *)
(*         "[Xml_file_classifier]: process: classifying anomalies" *)
(*         (fun _ -> *)
(*            Int_map.mapi *)
(*              (fun indice _ -> *)
(*                 ( *)
(*                   let mawilab_description_for_xml_classification = *)
(*                     HT.find *)
(*                       mawilab_description_for_xml_classification_h *)
(*                       indice *)
(*                   in *)

(*                   let detailed_metrics_string = *)
(*                     mawilab_description_for_xml_classification.Xml_classification_description.detailed_metrics_string *)
(*                   in *)

(*                   let network_traffic_attributes = *)
(*                     mawilab_description_for_xml_classification.Xml_classification_description.network_traffic_attributes *)
(*                   in *)
(*                   let network_traffic_values = *)
(*                     mawilab_description_for_xml_classification.Xml_classification_description.network_traffic_values *)
(*                   in *)

(*                   let anomaly_string = *)
(*                     (Base.Anomaly.to_string *)
(*                        (\* To_string_mode.Simple *\) *)
(*                        (Int_map.find *)
(*                           indice *)
(*                           anomaly_int_map *)
(*                        ) *)
(*                     ) *)
(*                     ^ "\n\n" ^ detailed_metrics_string *)
(*                   in *)

(*                   (Int_map.find *)
(*                      indice *)
(*                      anomaly_int_map *)
(*                   ) *)
(*                   , *)
(*                   mawilab_description_for_xml_classification *)
(*                   , *)
(*                   Anomaly_taxonomy_manager.classify *)
(*                     anomaly_taxonomy_manager *)
(*                     anomaly_string *)
(*                     network_traffic_attributes *)
(*                     network_traffic_values *)
(*                 ) *)
(*              ) *)
(*              anomaly_int_map *)
(*         ) *)
(*     in *)

(*     (\* debug *\) *)
(*     (\*   "process: signatures:\n%s" *\) *)
(*     (\*   (to_string_int_map *\) *)
(*     (\*      ~first: "" *\) *)
(*     (\*      ~last: "" *\) *)
(*     (\*      ~sep: "\n\n" *\) *)
(*     (\*      ~sep_key_value: ": " *\) *)
(*     (\*      string_of_int *\) *)
(*     (\*      (fun (anomaly , detailed_metrics , network_attributes, anomaly_signature) -> *\) *)
(*     (\*        sprintf *\) *)
(*     (\*          "%s\n%s\n\n%s\n%s" *\) *)
(*     (\*          (Mawilab_admd.Anomaly.to_string To_string_mode.Simple anomaly) *\) *)
(*     (\*          (Detailed_metrics.to_string To_string_mode.Simple detailed_metrics) *\) *)
(*     (\*          (Network_attributes.to_string To_string_mode.Normal network_attributes) *\) *)
(*     (\*          (Anomaly_signature.to_string To_string_mode.Command anomaly_signature) *\) *)
(*     (\*      ) *\) *)
(*     (\*      anomaly_network_attributes_anomaly_signature_tuple_int_map *\) *)
(*     (\*   ) *\) *)
(*     (\* ; *\) *)

(*     let mawilab_anomaly_mod_int_map = *)
(*       Int_map.map *)
(*         (fun *)
(*           ( *)
(*             mawilab_anomaly, *)
(*             mawilab_admd_mod_description, *)
(*             anomaly_signature *)
(*           ) *)
(*           -> *)
(*             ( *)
(*               let mawilab_anomaly_value = mawilab_anomaly.Base.Anomaly.anomaly_value in *)

(*               (\* let mawilab_anomaly_mod_value = *\) *)
(*               (\*   Mawilab_mod_anomaly_value.new_t *\) *)
(*               (\*       mawilab_anomaly_value.Mawilab_value.distance_to_normal *\) *)
(*               (\*       mawilab_anomaly_value.Mawilab_value.distance_to_abnormal *\) *)

(*               (\*       mawilab_anomaly_value.Mawilab_value.heuristic_detailed_anomaly_type *\) *)

(*               (\*       mawilab_anomaly_value.Mawilab_value.mawilab_detector_detection_result *\) *)

(*               (\*       anomaly_signature *\) *)
(*               (\* in *\) *)

(*               let mawilab_anomaly_mod_value = *)
(*                 mawilab_anomaly_value *)
(*                 ^ "," *)
(*                 ^ (Anomaly_signature.to_string To_string_mode.Command anomaly_signature) *)
(*               in *)


(*               let description_option = Some (mawilab_admd_mod_description) in *)

(*               Admd_mawilab_type_base_value_xml_description.Anomaly.new_t *)
(*                 mawilab_anomaly.Base.Anomaly.indice *)

(*                 mawilab_anomaly.Base.Anomaly.date *)
(*                 mawilab_anomaly.Base.Anomaly.time *)

(*                 (Mawilab_anomaly_type.of_string mawilab_anomaly.Base.Anomaly.anomaly_type) *)
(*                 mawilab_anomaly_mod_value *)
(*                 description_option *)

(*                 mawilab_anomaly.Base.Anomaly.slice_list *)

(*                 mawilab_anomaly.Base.Anomaly.start_time *)
(*                 mawilab_anomaly.Base.Anomaly.end_time *)
(*             ) *)
(*         ) *)
(*         ple_int_map *)
(*     in *)

(*     let mawilab_anomaly_mod_list = *)
(*       List.map *)
(*         snd *)
(*         (Int_map.bindings *)
(*            mawilab_anomaly_mod_int_map *)
(*         ) *)
(*     in *)

(*     let mawilab_anomaly_mod_container = *)
(*       Admd_mawilab_type_base_value_xml_description.Anomaly_container.new_t *)
(*         mawilab_anomaly_mod_list *)
(*     in *)

(*     let attributes_metadata_string = *)
(*       Sexplib.Sexp.to_string *)
(*         (Feature_name_container.sexp_of_t *)
(*            (Network_traffic_attributes.generate_feature_name_container *)
(*               () *)
(*            ) *)
(*         ) *)
(*     in *)

(*     let analysis = *)
(*       match mawilab_admd_file.Base.File.analysis_option with *)
(*       | None -> *)
(*         Admd_analysis.new_t *)
(*           attributes_metadata_string *)
(*           "" *)
(*           "" *)
(*           "" *)
(*       | Some analysis -> *)
(*         Admd_analysis.new_t *)
(*           (analysis.Admd_analysis.description *)
(*            ^ "--" ^ attributes_metadata_string) *)
(*           "" *)
(*           "" *)
(*           "" *)
(*     in *)

(*     let mawilab_anomaly_mod_file_filename = *)
(*       xml_file_basename_no_extension ^ "_cl.xml" *)
(*     in *)

(*     debug *)
(*       "process: xml_file_basename_no_extension/mawilab_anomaly_mod_file:\n%s\n%s" *)
(*       xml_file_basename_no_extension *)
(*       mawilab_anomaly_mod_file_filename; *)

(*     let mawilab_anomaly_mod_file = *)
(*       Admd_mawilab_type_base_value_xml_description.File.new_t *)
(*         mawilab_anomaly_mod_file_filename *)

(*         mawilab_admd_file.Base.File.date *)
(*         mawilab_admd_file.Base.File.time *)

(*         mawilab_admd_file.Base.File.algorithm_option *)
(*         ( *)
(*           match export_metrics_attributes with *)
(*           | false -> mawilab_admd_file.Base.File.analysis_option *)
(*           | true -> Some analysis *)
(*         ) *)
(*         mawilab_admd_file.Base.File.dataset_option *)

(*         mawilab_anomaly_mod_container *)
(*     in *)

(*     Admd_mawilab_type_base_value_xml_description.File.to_filename *)
(*       (\* ( *\) *)
(*       (\*   match export_metrics_attributes with *\) *)
(*       (\*   | false -> To_string_mode.Command *\) *)
(*       (\*   | true -> To_string_mode.Simple *\) *)
(*       (\* ) *\) *)
(*       mawilab_anomaly_mod_file; *)

(*     let signature_int_map = *)
(*       Int_map.map *)
(*         (fun *)
(*           ( *)
(*             mawilab_anomaly, *)
(*             mawilab_admd_mod_description, *)
(*             anomaly_signature *)
(*           ) *)
(*           -> *)
(*             anomaly_signature *)
(*         ) *)
(*         ple_int_map *)
(*     in *)

(*     (\* mawilab_anomaly_mod_file, *\) *)
(*     signature_int_map *)
(*   ) *)

let process
    anomaly_taxonomy

    export_metrics_attributes

    (* xml_file_basename_no_extension *)
    detailed_metrics_string_h
    anomaly_network_traffic_attributes_values_container

    mawilab_admd_file
    (* mawilab_description_for_xml_classification_int_map *)
  =
  (
    let anomaly_taxonomy_manager =
      Anomaly_taxonomy_manager.of_anomaly_taxonomy
        anomaly_taxonomy
    in

    let anomaly_container = mawilab_admd_file.Base.File.anomaly_container in

    let anomaly_list = anomaly_container.Base.Anomaly_container.anomaly_list in

    let anomaly_int_map =
      List.fold_left
        (fun int_map anomaly ->
           (
             Int_map.add
               anomaly.Base.Anomaly.indice
               anomaly
               int_map
           )
        )
        Int_map.empty
        anomaly_list
    in

    let ple_int_map =
      Execution_time_measure.execute
        "[Xml_file_classifier]: process: classifying anomalies"
        (fun _ ->
           Int_map.mapi
             (fun indice _ ->
                (
                  (* let mawilab_description_for_xml_classification = *)
                  (*   Int_map.find *)
                  (*     indice *)
                  (*     mawilab_description_for_xml_classification_int_map *)
                  (* in *)

                  (* let detailed_metrics_string = *)
                  (*   mawilab_description_for_xml_classification.Xml_classification_description.detailed_metrics_string *)
                  (* in *)

                  let detailed_metrics_string =
                    HT.find
                      detailed_metrics_string_h
                      indice
                  in

                  let network_traffic_attributes, network_traffic_values =
                    Anomaly_network_traffic_attributes_values_container.find
                      indice
                      anomaly_network_traffic_attributes_values_container
                  in

                  let anomaly_string =
                    (Base.Anomaly.to_string
                       (* To_string_mode.Simple *)
                       (Int_map.find
                          indice
                          anomaly_int_map
                       )
                    )
                    ^ "\n\n" ^ detailed_metrics_string
                  in

                  (Int_map.find
                     indice
                     anomaly_int_map
                  )
                  ,
                  Anomaly_taxonomy_manager.classify
                    anomaly_taxonomy_manager
                    anomaly_string
                    network_traffic_attributes
                    network_traffic_values
                )
             )
             anomaly_int_map
        )
    in

    let signature_int_map =
      Int_map.map
        (fun
          (
            mawilab_anomaly,
            (* mawilab_admd_mod_description, *)
            anomaly_signature
          )
          ->
            anomaly_signature
        )
        ple_int_map
    in

    (* mawilab_anomaly_mod_file, *)
    signature_int_map
  )





  
