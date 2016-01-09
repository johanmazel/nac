
let process
    global_feature_container
    anomaly_taxonomy
    network_attributes
  =
  Anomaly_taxonomy.fold
    (fun anomaly_signature anomaly_signature_ptree_list anomaly_signature_list ->
       let result =
         Network_attribute_signature_comparator.compare
           global_feature_container
           network_attributes
           anomaly_signature
       in

       if result then
         anomaly_signature :: anomaly_signature_list
       else
         anomaly_signature_list
    )
    anomaly_taxonomy
