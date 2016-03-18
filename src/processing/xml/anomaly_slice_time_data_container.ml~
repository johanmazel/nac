
open Printf

type t =
  {
    detailed_metrics : Detailed_metrics.t;
    network_traffic_attributes : Network_traffic_attributes.t;
    network_traffic_values : Network_traffic_values.t;
  }

let new_t
    detailed_metrics
    network_traffic_attributes
    network_traffic_values
    =
  {
    detailed_metrics;
    network_traffic_attributes;
    network_traffic_values;
  }

let of_trace_statistics_detailed_metrics
    trace_statistics
    detailed_metrics
    =
  new_t
    detailed_metrics
    (Network_traffic_attributes.of_trace_statistics_detailed_metrics trace_statistics detailed_metrics)
    (Network_traffic_values.of_trace_statistics_detailed_metrics trace_statistics detailed_metrics)

let fusion t1 t2 =
  failwith "Classification_data: CANNOT use fusion"

let to_string t =
  failwith "Classification_data: CANNOT use to_string"
