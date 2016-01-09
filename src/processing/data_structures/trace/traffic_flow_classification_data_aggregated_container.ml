
open Printf

module HT = BatHashtbl

open Traffic_flow_aggr_data_instantiations

let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
  (fun s -> Format.printf "[Traffic_flow_detailed_metrics_aggregated_container]: %s@." s)
      else
  ignore
    )
    fmt

type t =
  {
    h : (Traffic_flow_key_type.aggr_mode_t, ((Traffic_flow_key_type.aggr_t, Classification_data.t) HT.t)) Hashtbl.t;
  }

let new_t
    h
  =
  {
    h;
  }

let length t = 
  HT.length
    t.h

let length_total t = 
  HT.fold
    (fun aggr_key traffic_flow_classification_data_aggr_data total ->
       total + (HT.length traffic_flow_classification_data_aggr_data)
    )
    t.h
    0

let map_to_hashtbl
    f
    t
  =
  HT.map
    f
    t.h

let of_traffic_flow_detailed_metrics_aggregated_container
    trace_statistics
    traffic_flow_detailed_metrics_aggregated_container
  =
  let hashtable = 
    Batteries.Hashtbl.map
      (fun aggr_mode traffic_flow_detailed_metrics_key_aggr_data_container ->
         (
           let hashtable_temp = Traffic_flow_detailed_metrics_aggr_data.Aggr_key_data_container.map_to_hashtbl
               (fun detailed_metrics ->
                  Classification_data.new_t
                    detailed_metrics
                    (Network_traffic_attributes.of_trace_statistics_detailed_metrics
                       trace_statistics detailed_metrics
                    )
                    (Network_traffic_values.of_trace_statistics_detailed_metrics
                       trace_statistics detailed_metrics
                    )
               )
               traffic_flow_detailed_metrics_key_aggr_data_container
           in

           hashtable_temp
         )
      )
      traffic_flow_detailed_metrics_aggregated_container.Traffic_flow_detailed_metrics_aggregated_container.hashtable
  in

  new_t
    hashtable

