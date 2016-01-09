
let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Network_attribute_signature_comparator]: %s@." s)
      else
        ignore
    )
    fmt

let compare
(* global_feature_container *)
    network_attributes
    anomaly_signature
=
  (
    debug "compare: call";
    
    debug
      "compare: network_attributes:\n%s"
      (Network_attributes.to_string To_string_mode.Normal network_attributes);
    
    debug
      "compare: anomaly_signature:\n%s"
      (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature);
    
    (* let feature_rule_result =                                                          *)
    (*   Anomaly_signature.fold_feature_rule                                              *)
    (*     (fun feature feature_rule anomaly_present ->                                   *)
    (*           debug                                                                    *)
    (*             "compare: feature: %s"                                                 *)
    (*             (Feature.to_string To_string_mode.Normal feature);                     *)
    
    (*           let feature_indice = feature_rule.Feature_rule.feature.Feature.indice in *)
    (*           let attribute_value =                                                    *)
    (*             Network_attributes.find_attribute                                      *)
    (*               network_attributes                                                   *)
    (*               feature_indice                                                       *)
    (*           in                                                                       *)
    
    (*           debug                                                                    *)
    (*             "compare: attribute_value: %f"                                         *)
    (*             attribute_value;                                                       *)
    
    (*           let result =                                                             *)
    (*             Feature_rule.test_value                                                *)
    (*               feature_rule                                                         *)
    (*               attribute_value                                                      *)
    (*           in                                                                       *)
    
    (*           anomaly_present && result                                                *)
    (*     )                                                                              *)
    (*     anomaly_signature                                                              *)
    (*     true                                                                           *)
    (* in                                                                                 *)
    
    let result =
      Anomaly_signature.fold
        (fun rule ->
              match rule with
              | Rule.Feature feature_rule ->
                  let feature = feature_rule.Feature_rule.feature in
                  
                  debug
                    "compare: feature: %s"
                    (Feature.to_string To_string_mode.Normal feature);
                  
                  let feature_indice = feature_rule.Feature_rule.feature.Feature.indice in
                  let attribute_value =
                    Network_attributes.find_attribute
                      network_attributes
                      feature_indice
                  in
                  
                  debug
                    "compare: attribute_value: %f"
                    attribute_value;
                  
                  let result =
                    Feature_rule.test_value
                      feature_rule
                      attribute_value
                  in
                  result
              | Rule.Metric metric_rule ->
                  assert(false)
          
        )
        Boolean_operator.apply
        anomaly_signature
    in
    
    debug "compare: result: %b" result;
    
    (* Note: if any of the metric_rule are positive, the result is positive *)
    
    let metric_rule_result =
      if Anomaly_signature.length_metric_rule anomaly_signature = 0 then
        true
      else
        Anomaly_signature.fold_metric_rule
          (fun feature metric_rule anomaly_present ->
                let metric_indice = metric_rule.Metric_rule.metric.Metric.indice in
                let metric_value =
                  Network_attributes.find_metric
                    network_attributes
                    metric_indice
                in
                
                let result =
                  Metric_rule.test_value
                    metric_rule
                    metric_value
                in
                
                anomaly_present || result
          )
          anomaly_signature
          false
    in
    
    debug "compare: metric_rule_result: %b" metric_rule_result;
    
    let final_result = feature_rule_result && metric_rule_result in
    
    debug "compare: result: %b" final_result;
    
    debug "compare: call";
    
    final_result
  )

(* let test_signature t signature =                                                                  *)
(*   (                                                                                               *)
(*     let ref_anomaly_present = ref false in                                                        *)

(*     let get_value_of_attribute_id attribute_id =                                                  *)
(*       match attribute_id.Feature.indice with                                                      *)
(*       | 0 -> t.nb_destinations                                                                    *)
(*       | 1 -> t.nb_sources                                                                         *)
(*       | 2 -> t.nb_packets_over_nb_diff_values_dest_port                                           *)
(*       | 3 -> t.nb_diff_src_addr_over_nb_diff_dest_addr                                            *)
(*       | 4 -> t.nb_icmp_packets_over_nb_packets                                                    *)
(*       | 5 -> t.nb_echorequestreply_packets_over_nb_packets                                        *)
(*       | 6 -> t.nb_syn_packets_over_nb_packets                                                     *)
(*       | 7 -> t.nb_rst_packets_over_nb_packets                                                     *)
(*       | 8 -> t.biggest_dest_port_over_every_other_port                                            *)
(*       | 9 -> t.avg_nb_dest_port                                                                   *)
(*       | _ -> 0.0                                                                                  *)
(*     in                                                                                            *)

(*     let test_rule rule =                                                                          *)
(*       (                                                                                           *)
(*         ref_anomaly_present := !ref_anomaly_present &&                                            *)
(*         (                                                                                         *)
(*           match rule with                                                                         *)
(*           | Rule.Relative_rule relative_rule ->                                                   *)
(*               (                                                                                   *)
(*                 let value = get_value_of_attribute_id relative_rule.Relative_rule.attribute_id in *)

(*                 Relative_rule.test_value relative_rule value                                      *)
(*               )                                                                                   *)
(*           | Rule.Absolute_rule absolute_rule -> true                                              *)

(*         )                                                                                         *)
(*         ;                                                                                         *)
(*       )                                                                                           *)
(*     in                                                                                            *)

(*     Signature.iter                                                                                *)
(*       test_rule                                                                                   *)
(*       signature;                                                                                  *)

(*   )                                                                                               *)
