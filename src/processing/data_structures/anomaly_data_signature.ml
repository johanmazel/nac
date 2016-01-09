
open Printf

type t =
  {
    detailed_metrics : Detailed_metrics.t;
    network_traffic_attributes : Network_traffic_attributes.t;
    network_traffic_values : Network_traffic_values.t;
    anomaly_raw_data : Anomaly_raw_data.t;
    anomaly_signature : Anomaly_signature.t;
  }

let new_t
    detailed_metrics
    network_traffic_attributes
    network_traffic_values
    anomaly_raw_data
    anomaly_signature
    =
  {
    detailed_metrics;
    network_traffic_attributes;
    network_traffic_values;
    anomaly_raw_data;
    anomaly_signature;
  }

let fusion t1 t2 =
  failwith "Classification_data: CANNOT use fusion"

let to_string t =
  failwith "Classification_data: CANNOT use to_string"
