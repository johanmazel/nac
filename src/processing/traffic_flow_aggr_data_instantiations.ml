

module Traffic_flow_five_tuple_flow_detailed_metrics_aggr_data = 
  Aggr_data.Make(Traffic_flow_key_type)(Five_tuple_flow_metrics)

module Traffic_flow_detailed_metrics_aggr_data =
  Aggr_data.Make(Traffic_flow_key_type)(Detailed_metrics)






