
open Printf

(* open Traffic_flow_aggr_data_functor_instantiations *)
open Traffic_flow_aggr_data_functor_instantiations

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
    hashtable : (Traffic_flow_key_type.aggr_mode_t, Traffic_flow_network_attributes_aggr_data_functor.Key_aggr_data_container.t) Hashtbl.t;
  }

let new_t
    hashtable
  =
  {
    hashtable = hashtable;
  }

let length t = Hashtbl.length t.hashtable

let map_to_hashtbl
    f
    t
  =
  Batteries.Hashtbl.map
    f
    t.hashtable

let of_traffic_flow_detailed_metrics_aggregated_container
    traffic_flow_detailed_metrics_aggregated_container
  =
  let hashtable = 
    Batteries.Hashtbl.map
      (fun aggr_mode traffic_flow_detailed_metrics_key_aggr_data_container ->
         (
           let key_aggr_mode =
             traffic_flow_detailed_metrics_key_aggr_data_container.Traffic_flow_detailed_metrics_aggr_data_functor.Key_aggr_data_container.key_aggr_mode 
           in

           let hashtable_temp = Traffic_flow_detailed_metrics_aggr_data_functor.Key_aggr_data_container.map_to_hashtbl
               Network_attributes.of_detailed_metrics
               traffic_flow_detailed_metrics_key_aggr_data_container
           in

           Traffic_flow_network_attributes_aggr_data_functor.Key_aggr_data_container.of_aggr_data_hashtable
             key_aggr_mode
             hashtable_temp
         )
      )
      traffic_flow_detailed_metrics_aggregated_container.Traffic_flow_detailed_metrics_aggregated_container.hashtable
  in

  new_t
    hashtable
