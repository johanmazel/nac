
open Printf

module L = BatList
module HT = BatHashtbl

open Admd.Instantiation
    
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
        (fun s -> Format.printf "[Data_hashtable_builder]: %s@." s)
      else
        ignore
    )
    fmt

let process
    trace_statistics
    anomaly_detailed_metrics_container 

    anomaly_container
  =
  debug "process: call";

  debug "process: generating network_attributes";

  let anomaly_network_attributes_values_container =
    Execution_time_measure.execute
      "[Data_hashtable_builder]: process: building attributes"
      (fun _ ->
         Anomaly_network_traffic_attributes_values_container.of_trace_statistics_anomaly_detailed_metrics_container
           trace_statistics
           anomaly_detailed_metrics_container
      )
  in

  let anomaly_list = Base.Anomaly_container.to_list anomaly_container in
  let anomaly_h =
    HT.of_enum
      (L.enum
         (L.map
            (fun anomaly ->
               anomaly.Base.Anomaly.indice,
               anomaly
            )
            anomaly_list
         )
      )
  in
  let initial_description_string_h =
    HT.map
      (fun _ anomaly ->
         match anomaly.Base.Anomaly.anomaly_description_option with
         | None -> ""
         | Some string -> string
      )
      anomaly_h
  in

  debug "get_int_map: generating detailed_metrics_string";

  (* let detailed_metrics_string_h = *)
  (*   Anomaly_detailed_metrics_container.fold_detailed_metrics *)
  (*     (fun indice detailed_metrics h_acc -> *)
  (*        HT.add *)
  (*          h_acc *)
  (*          indice *)
  (*          (Detailed_metrics.to_string To_string_mode.Simple detailed_metrics) *)
  (*        ; *)

  (*        h_acc *)
  (*     ) *)
  (*     anomaly_detailed_metrics_container *)
  (*     (HT.create 0) *)
  (* in *)
  let detailed_metrics_string_h =
    HT.map
      (fun indice detailed_metrics ->
         Detailed_metrics.to_string detailed_metrics
      )
      (HT.of_enum
         (L.enum
            (Anomaly_detailed_metrics_container.to_list_detailed_metrics
               anomaly_detailed_metrics_container
            )
         )
      )
  in

  debug "process: generating Mawilab_mod_anomaly_description";
  let mawilab_admd_mod_description_h =
    HT.map
      (fun indice initial_description_string ->
         let network_traffic_attributes, network_traffic_values =
           Anomaly_network_traffic_attributes_values_container.find
             indice
             anomaly_network_attributes_values_container
         in

         (* let anomaly_raw_data = Int_map.find indice anomaly_raw_data_int_map in *)
         Xml_classification_description.new_t
           initial_description_string
           (HT.find detailed_metrics_string_h indice)
           (Anomaly_detailed_metrics_container.find_anomaly_metric
              indice
              anomaly_detailed_metrics_container)

           network_traffic_attributes
           network_traffic_values
           (* anomaly_raw_data.Anomaly_raw_data.anomaly_ip_address *)
      )
      initial_description_string_h
  in

  debug "process: generating anomaly_raw_data";
  let anomaly_raw_data_int_map =
    Anomaly_detailed_metrics_container.fold_detailed_metrics
      (fun indice detailed_metrics int_map ->
         Int_map.add
           indice
           (Anomaly_raw_data.of_detailed_metrics detailed_metrics)
           int_map
      )
      anomaly_detailed_metrics_container
      Int_map.empty
  in

  debug "process: end";

  initial_description_string_h,
  detailed_metrics_string_h,

  anomaly_network_attributes_values_container,
  anomaly_raw_data_int_map,

  mawilab_admd_mod_description_h
  
