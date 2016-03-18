
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





  
