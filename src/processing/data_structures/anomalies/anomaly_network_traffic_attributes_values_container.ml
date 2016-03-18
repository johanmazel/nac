
open Printf

module L = BatList
module HT = BatHashtbl

open Map_ext_instantiations

let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Anomaly_detailed_metrics_container]: %s@." s)
      else
        ignore
    )
    fmt

type t =
  {
    map : (Network_traffic_attributes.t * Network_traffic_values.t) Int_map.t;
  }

let new_t
    map
    =
  {
    map;
  }

let to_string t =
  Map_ext_instantiations.Int_map.to_string
    ~sep: "\n\n"
    (fun indice (network_traffic_attributes, _) ->
       sprintf
         "%d: %s"
         indice
         (Network_traffic_attributes.to_string network_traffic_attributes))
    t.map
  (* match to_string_mode with *)
  (* | To_string_mode.Command -> *)
  (*   Map_ext_instantiations.Int_map.to_string *)
  (*     ~sep: "\n\n" *)
  (*     (fun indice (network_traffic_attributes, _) -> *)
  (*        sprintf *)
  (*          "%d: %s" *)
  (*          indice *)
  (*          (Network_traffic_attributes.to_string network_traffic_attributes)) *)
  (*     t.map *)
  (* | To_string_mode.Simple -> *)
  (*   Map_ext_instantiations.Int_map.to_string *)
  (*     ~sep: "\n\n" *)
  (*     (fun indice (network_traffic_attributes, _) -> *)
  (*        sprintf *)
  (*          "%d: %s" *)
  (*          indice *)
  (*          (Network_traffic_attributes.to_string network_traffic_attributes) *)
  (*     ) *)
  (*     t.map *)
  (* | To_string_mode.Normal -> *)
  (*   Map_ext_instantiations.Int_map.to_string *)
  (*     ~sep: "\n\n" *)
  (*     (fun indice (network_traffic_attributes, _) -> *)
  (*        sprintf *)
  (*          "%d: %s" *)
  (*          indice *)
  (*          (Network_traffic_attributes.to_string network_traffic_attributes)) *)
  (*     t.map *)

let of_trace_statistics_anomaly_detailed_metrics_container
    trace_statistics
    anomaly_detailed_metrics_container
  =
  (* let detailed_metrics_int_map = *)
  (*   Hashtbl.fold *)
  (*     (fun int tuple int_map -> *)
  (*        Int_map.add *)
  (*          int  *)
  (*          tuple *)
  (*          int_map *)
  (*     ) *)
  (*     anomaly_detailed_metrics_container.Anomaly_detailed_metrics_container.detailed_metrics_h *)
  (*     Int_map.empty *)
  (* in *)

  (* let int_map = *)
  (*   Int_map.map *)
  (*     (fun detailed_metrics -> *)
  (*        Network_traffic_attributes.of_trace_statistics_detailed_metrics trace_statistics detailed_metrics, *)
  (*        Network_traffic_values.of_trace_statistics_detailed_metrics trace_statistics detailed_metrics *)
  (*     ) *)
  (*     detailed_metrics_int_map *)
  (* in *)

  let h =
    HT.of_enum
      (L.enum
         (L.map
            (fun (indice, detailed_metrics) ->
               indice
               ,
               (
                 Network_traffic_attributes.of_trace_statistics_detailed_metrics
                   trace_statistics
                   detailed_metrics,
                 Network_traffic_values.of_trace_statistics_detailed_metrics
                   trace_statistics
                   detailed_metrics
               )
            )
            (Anomaly_detailed_metrics_container.to_list_detailed_metrics
               anomaly_detailed_metrics_container
            )
         )
      )
  in

  let int_map =
    Int_map.of_list
      (L.of_enum
         (HT.enum h)
      )
  in

  new_t
    int_map

let to_int_map t = t.map

let find indice t = Int_map.find indice t.map
