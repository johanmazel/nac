
open Printf

module L = BatList
module HT = BatHashtbl

open Traffic_flow_aggr_data_instantiations

let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
  (fun s -> Format.printf "[Traffic_flow_anomaly_data_signature_aggregated_container]: %s@." s)
      else
  ignore
    )
    fmt

type t =
  {
    h : (Traffic_flow_key_type.aggr_mode_t, ((Traffic_flow_key_type.aggr_t, Anomaly_data_signature.t) HT.t)) Hashtbl.t;
  }

let new_t
   h
  =
  {
    h;
  }

let length t = HT.length t.h

let map_to_hashtbl
    f
    t
  =
  HT.map
    f
    t.h

let fold
    f
    t
  =
  HT.fold
    f
    t.h

let of_traffic_flow_classification_data_aggregated_container
    parallelization_mode
    anomaly_taxonomy
    traffic_flow_classification_data_aggregated_container
  =
  debug "of_traffic_flow_classification_data_aggregated_container: call";

  let anomaly_taxonomy_manager =
    Anomaly_taxonomy_manager.of_anomaly_taxonomy
      anomaly_taxonomy
  in

  let hashtable = 
    HT.map
      (fun
        key_aggr_mode 
        h
        ->
          (
            let anomaly_signature_matching_mode =
              match key_aggr_mode with
              | Traffic_flow_key_type.All_mode -> failwith "Traffic_flow_anomaly_data_signature_aggregated_container: of_traffic_flow_classification_data_aggregated_container: cannot calssify anomalies when traffic is completly aggregated (All)"
              | Traffic_flow_key_type.Src_addr_mode -> Anomaly_signature_matching_mode.Src
              | Traffic_flow_key_type.Dst_addr_mode -> Anomaly_signature_matching_mode.Dst
            in

            let tuple_list =
              L.of_enum
                (HT.enum
                   h
                )
            in

            let tuple_list_mapped =
              Map_data.map_list
                parallelization_mode
                (fun (aggr_key, classification_data) ->
                   (aggr_key
                    ,
                    (
                      classification_data.Classification_data.detailed_metrics
                      ,
                      classification_data.Classification_data.network_traffic_attributes
                      ,
                      classification_data.Classification_data.network_traffic_values
                      ,
                      (Anomaly_taxonomy_manager.classify_option
                         anomaly_taxonomy_manager
                         (Network_traffic_attributes.to_string
                            classification_data.Classification_data.network_traffic_attributes)
                         anomaly_signature_matching_mode
                         classification_data.Classification_data.network_traffic_attributes
                         classification_data.Classification_data.network_traffic_values
                      )
                    )
                   )
                )
                tuple_list
            in

            let hashtable_temp =
              Batteries.Hashtbl.of_enum
                (Batteries.List.enum
                   tuple_list_mapped
                )
            in

            let hashtable_temp_filtered =
              Batteries.Hashtbl.filter
                (fun (_, _, _, anomaly_signature_option) ->
                   match anomaly_signature_option with
                   | None -> false
                   | Some _ -> true
                )
                hashtable_temp
            in
            let hashtable_temp_filtered_mapped =
              Batteries.Hashtbl.map
                (fun key_aggr (detailed_metrics, network_traffic_attributes, network_traffic_values, anomaly_signature_option) ->
                   match anomaly_signature_option with
                   | None -> assert(false)
                   | Some anomaly_signature -> (detailed_metrics, network_traffic_attributes, network_traffic_values, anomaly_signature)
                )
                hashtable_temp_filtered
            in

            let hashtable =
              Batteries.Hashtbl.map
                (fun key_aggr (detailed_metrics, network_traffic_attributes, network_traffic_values, anomaly_signature) ->

                   let anomaly_raw_data =
                     Anomaly_raw_data.of_detailed_metrics
                       detailed_metrics
                   in

                   Anomaly_data_signature.new_t
                     detailed_metrics
                     network_traffic_attributes
                     network_traffic_values
                     anomaly_raw_data
                     anomaly_signature
                )
                hashtable_temp_filtered_mapped
            in

            hashtable
          )
      )
      traffic_flow_classification_data_aggregated_container.Traffic_flow_classification_data_aggregated_container.h
  in

  debug "of_traffic_flow_classification_data_aggregated_container: end";

  new_t
    hashtable

