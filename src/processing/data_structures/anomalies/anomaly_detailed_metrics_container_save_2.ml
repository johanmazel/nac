
open Printf

open Map_ext_instantiations

open Ipv4
open Tcp
open Udp
open Icmp

open Admd_functor_instantiation
open Mawilab_admd_functor_instantiation

open Key_occurrence_distribution_instantiations

let debug_enabled = ref true

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
    detailed_metrics_int_map : (int, Detailed_metrics.t) Hashtbl.t;
  }

let new_t
    detailed_metrics_int_map
    =
  {
    detailed_metrics_int_map = detailed_metrics_int_map;
  }

let to_string to_string_mode t =
  match to_string_mode with
  | To_string_mode.Command ->
    Utils_batteries.to_string_hashtbl
      ~sep_element: "\n\n"
      ~sep_key_value: ": "
      ~to_string_key: (fun key -> sprintf "%d" key)
      (fun detailed_metrics -> Detailed_metrics.to_string to_string_mode detailed_metrics)
      t.detailed_metrics_int_map
  | To_string_mode.Simple ->
    Utils_batteries.to_string_hashtbl
      ~sep_element: "\n\n"
      ~sep_key_value: ": "
      ~to_string_key: (fun key -> sprintf "%d" key)
      (fun detailed_metrics -> Detailed_metrics.to_string to_string_mode detailed_metrics)
      t.detailed_metrics_int_map
  | To_string_mode.Normal ->
    Utils_batteries.to_string_hashtbl
      ~sep_element: "\n\n"
      ~sep_key_value: ": "
      ~to_string_key: (fun key -> sprintf "%d" key)
      (fun detailed_metrics -> Detailed_metrics.to_string to_string_mode detailed_metrics)
      t.detailed_metrics_int_map

let length t = Hashtbl.length t.detailed_metrics_int_map

let find indice t = Hashtbl.find t.detailed_metrics_int_map indice 

let fold f t acc = Hashtbl.fold f t.detailed_metrics_int_map acc

let of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map
    parallelization_mode
    five_tuple_flow_metrics_container
    anomaly_container
    =
  (
    debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: call";

    let five_tuple_flow_metrics_tuple_list =
      Five_tuple_flow_metrics_container.to_list
        five_tuple_flow_metrics_container
    in

    let nb_anomaly = Mawilab_admd.Anomaly_container.length anomaly_container in

    let fusion_map (hashtable_1 : (int, Detailed_metrics.t) Hashtbl.t) hashtable_2 =
      debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: fusion_map: call";

      let detailed_metrics_int_map_1 =
        Hashtbl.fold
          (fun int detailed_metrics int_map ->
            Int_map.add
              int
              detailed_metrics
              int_map
          )
          hashtable_1
          Int_map.empty
      in

      let detailed_metrics_int_map_2 =
        Hashtbl.fold
          (fun int detailed_metrics int_map ->
            Int_map.add
              int
              detailed_metrics
              int_map
          )
          hashtable_2
          Int_map.empty
      in

      let int_map =
        Int_map.merge
          (fun five_tuple_flow detailed_metrics_option_1 detailed_metrics_option_2 ->
            match detailed_metrics_option_1 with
            | None ->
              (
                match detailed_metrics_option_2 with
                | None -> failwith "Anomaly_detailed_metrics_container: apply_anomaly_container_five_tuple_flow_detailed_metrics_container: nothing a five_tuple_flow"
                | Some detailed_metrics_2 -> Some detailed_metrics_2
              )
            | Some detailed_metrics_1 ->
              (
                match detailed_metrics_option_2 with
                | None -> Some detailed_metrics_1
                | Some detailed_metrics_2 -> Some (Detailed_metrics.fusion detailed_metrics_1 detailed_metrics_2)
              )
          )
          detailed_metrics_int_map_1
          detailed_metrics_int_map_2
      in

      let new_hashtable = Hashtbl.create (Int_map.cardinal int_map) in

      Int_map.iter
        (fun indice detailed_metrics ->
          Hashtbl.add
            new_hashtable
            indice
            detailed_metrics
        )
        int_map;

      debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: fusion_map: end";

      new_hashtable
    in

    let parallelization_mode_to_use =
      if Five_tuple_flow_metrics_container.length five_tuple_flow_metrics_container < 5000000 then
  parallelization_mode
      else 
  Parallelization_mode.No_parallelization 2000
    in

    let new_detailed_metrics_int_map =
      Map_data.fold_list
        parallelization_mode_to_use
        (fun hashtable_1 (five_tuple_flow, five_tuple_flow_metrics) ->
          (
            let src_addr,
        dst_addr,
        proto,
        src_port,
        dst_port
              =
              Five_tuple_flow.to_five_tuple
                five_tuple_flow
            in

            let new_hashtable_1 =
              Mawilab_admd.Anomaly_container.fold_left
                (fun hashtable_2 anomaly ->
            (
              (* let _compare_result = *)
              (*   Mawilab_admd.Anomaly.match_flow_option *)
              (*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_sec_start *)
              (*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_usec_start *)
              (*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_sec_end *)
              (*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_usec_end *)
              (*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.nb_packets *)
              (*     src_ip_option *)
              (*     dst_ip_option *)
              (*     proto_option *)
              (*     src_port_option *)
              (*     dst_port_option *)
              (*     anomaly *)
        (* in *)
              
                    (* debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: comparing"; *)

              let compare_result =
                Mawilab_admd.Anomaly.match_flow
                  five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_start
                  five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_start
                  five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_end
                  five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_end
                        false
                  (* five_tuple_flow_metrics.Five_tuple_flow_metrics.nb_packets *)
                  src_addr
                  dst_addr
                  (Transport_protocol_translation.transport_protocol_for_metrics_to_admd_transport_protocol proto)
                  src_port
                  dst_port
                  anomaly
        in

                    (* debug *)
                    (*   "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map:\n%s\n%s" *)
                    (*   (Five_tuple_flow.to_string five_tuple_flow) *)
                    (*   (Mawilab_admd.Anomaly.to_string To_string_mode.Simple anomaly); *)

                    (* debug *)
                    (*   "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: %b\n\n" *)
                    (*   compare_result; *)

        let hashtable =
                if compare_result then
      (
                    let indice = anomaly.Mawilab_admd.Anomaly.indice in
        
        (* debug *)
        (*   "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: %d" *)
        (*   indice; *)

                          let new_hashtbl =
                            try
                              (
        let found_detailed_metrics =
                Hashtbl.find
                                    hashtable_2
                                    indice
        in

        (* debug *)
        (*   "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: found detaild_metrics:\n%s" *)
        (*   (Detailed_metrics.to_string *)
        (*      To_string_mode.Simple *)
        (*      found_detailed_metrics); *)

        (* Detailed_metrics.verify "" found_detailed_metrics; *)

        let detailed_metrics =
                                  Detailed_metrics.of_five_tuple_flow_metrics
                                    five_tuple_flow
                                    five_tuple_flow_metrics
        in
        Detailed_metrics.append
          found_detailed_metrics
          detailed_metrics;

        (* let updated_detailed_metrics = *)
                                (*   Detailed_metrics.fusion *)
                                (*     found_detailed_metrics *)
                                (*     detailed_metrics *)
        (* in *)
        (* Hashtbl.replace *)
                                (*   hashtable_2 *)
                                (*   indice *)
                                (*   updated_detailed_metrics; *)

        (* BUG: netscanUDNet become unknown *)
        (* Detailed_metrics.update_five_tuple_flow_detailed_metrics *)
        (*   found_detailed_metrics *)
        (*   five_tuple_flow *)
        (*   five_tuple_flow_detailed_metrics; *)

        hashtable_2
                              )
                            with
                            | Not_found ->
                              (
        let new_detailed_metrics =
                Detailed_metrics.of_five_tuple_flow_metrics
                  five_tuple_flow
                  five_tuple_flow_metrics
        in

        (* debug *)
        (*   "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: not found => new detaild_metrics:\n%s" *)
        (*   (Detailed_metrics.to_string *)
        (*      To_string_mode.Simple *)
        (*      new_detailed_metrics); *)

        (* Detailed_metrics.verify "" new_detailed_metrics; *)

        Hashtbl.add
                                  hashtable_2
                                  indice
                                  new_detailed_metrics;
        
        (* debug *)
        (*   "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: hashtable:\n%s" *)
        (*   (Utils_batteries.to_string_hashtbl *)
        (*      ~to_string_key: string_of_int *)
        (*      (Detailed_metrics.to_string To_string_mode.Simple) *)
        (*      hashtable_2 *)
        (*   ); *)

        hashtable_2
                              )
                          in

                          new_hashtbl
      )
                      else
      (
                          hashtable_2
      )
        in

        hashtable
                  )
    )
                hashtable_1
                anomaly_container
      in

      new_hashtable_1
          )
        )
        fusion_map
        (Hashtbl.create nb_anomaly)
        five_tuple_flow_metrics_tuple_list
    in

    (* Completing metrics for anomalies with empty traffic *)
    Mawilab_admd.Anomaly_container.iter
      (fun anomaly ->
        try 
          (
      let _detailed_metrics_found =
        Hashtbl.find
                new_detailed_metrics_int_map 
                anomaly.Mawilab_admd.Anomaly.indice
      in

      ()
          )
        with
        | Not_found ->
          (
      Hashtbl.add
        new_detailed_metrics_int_map
        anomaly.Mawilab_admd.Anomaly.indice 
        (Detailed_metrics.new_empty_t ())
          )
      )
      anomaly_container;

    (* t.detailed_metrics_int_map <- new_detailed_metrics_int_map; *)
    
    debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: end";

    new_t
      new_detailed_metrics_int_map

  (* t *)
  )

let of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_hashtable
    parallelization_mode
    five_tuple_flow_detailed_metrics_container
    anomaly_container
    =
  (
    debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_hashtbl: call";

    let five_tuple_flow_metrics_tuple_list =
      Five_tuple_flow_metrics_container.to_list
        five_tuple_flow_detailed_metrics_container
    in

    let nb_anomaly = Mawilab_admd.Anomaly_container.length anomaly_container in

    let fusion_hashtable hashtable_1 hashtable_2 =
      Hashtbl.iter
  (fun indice_2 detailed_metrics_2 ->
    (
      try(
        let detailed_metrics_1 =
    Hashtbl.find
      hashtable_1
                  indice_2
        in   

        Hashtbl.replace
    hashtable_1
    indice_2
    (Detailed_metrics.fusion detailed_metrics_1 detailed_metrics_2)
      );
      with
      | Not_found ->
        (
    Hashtbl.add
      hashtable_1
      indice_2
                  detailed_metrics_2;
        );
    )
  )
  hashtable_2;
      
      hashtable_1
    in

    let new_detailed_metrics_int_map =
      Map_data.fold_list
        parallelization_mode
        (fun hashtable_1 (five_tuple_flow, five_tuple_flow_metrics) ->
          (
            (* let src_ip_option *)
      (*     , dst_ip_option *)
      (*       , proto_option *)
      (*         , src_port_option *)
      (*         , dst_port_option *)
            (*             = *)
            (*   Five_tuple_flow.to_five_tuple_option *)
            (*     five_tuple_flow *)
            (* in *)
            (* let proto_option = *)
      (*   match proto_option with *)
      (*   | None -> None *)
      (*   | Some int -> Some (Transport_protocol.of_int int) *)
      (* in *)

            let src_addr,
        dst_addr,
        proto,
        src_port,
        dst_port
              =
              Five_tuple_flow.to_five_tuple
                five_tuple_flow
            in

            let new_hashtable_1 =
              Mawilab_admd.Anomaly_container.fold_left
                (fun hashtable_2 anomaly ->
            (
              (* let _compare_result = *)
              (*   Mawilab_admd.Anomaly.match_flow_option *)
              (*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_sec_start *)
              (*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_usec_start *)
              (*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_sec_end *)
              (*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_usec_end *)
              (*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.nb_packets *)
              (*     src_ip_option *)
              (*     dst_ip_option *)
              (*     proto_option *)
              (*     src_port_option *)
              (*     dst_port_option *)
              (*     anomaly *)
        (* in *)
              
              let compare_result =
                Mawilab_admd.Anomaly.match_flow
                  five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_start
                  five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_start
                  five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_end
                  five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_end
                        false
                  (* five_tuple_flow_metrics.Five_tuple_flow_metrics.nb_packets *)
                  src_addr
                  dst_addr
                  (Transport_protocol_translation.transport_protocol_for_metrics_to_admd_transport_protocol proto)
                  src_port
                  dst_port
                  anomaly
        in

              if compare_result then
                (
                  let indice = anomaly.Mawilab_admd.Anomaly.indice in
      
                        let new_hashtbl =
                          try
                            (
            let found_detailed_metrics =
              Hashtbl.find
                                  hashtable_2
                                  indice
            in

                              let detailed_metrics =
                                Detailed_metrics.of_five_tuple_flow_metrics
                                  five_tuple_flow
                                  five_tuple_flow_metrics
                              in

                              let updated_detailed_metrics =
                                Detailed_metrics.fusion
                                  found_detailed_metrics
                                  detailed_metrics
                              in

                              Hashtbl.replace
                                hashtable_2
                                indice
                                updated_detailed_metrics;

                              (* BUG: netscanUDNet become unknown *)
                              (* Detailed_metrics.update_five_tuple_flow_detailed_metrics *)
                              (*   found_detailed_metrics *)
                              (*   five_tuple_flow *)
                              (*   five_tuple_flow_detailed_metrics; *)

                              hashtable_2
                            )
                          with
                          | Not_found ->
                            (
            let new_detailed_metrics =
              Detailed_metrics.of_five_tuple_flow_metrics
                five_tuple_flow
                five_tuple_flow_metrics
            in

                              Hashtbl.add
                                hashtable_2
                                indice
                                new_detailed_metrics;
            
            hashtable_2
                            )
                        in

                        new_hashtbl
                )
                    else
                      (
                        hashtable_2
                      )
            )
                )
                hashtable_1
                anomaly_container
            in

            new_hashtable_1
          )
        )
        fusion_hashtable
        (Hashtbl.create nb_anomaly)
        five_tuple_flow_metrics_tuple_list
    in

    (* Completing metrics for anomalies with empty traffic *)
    Mawilab_admd.Anomaly_container.iter
      (fun anomaly ->
        try 
          (
            let _detailed_metrics_found =
              Hashtbl.find
                new_detailed_metrics_int_map 
                anomaly.Mawilab_admd.Anomaly.indice
            in

            ()
          )
        with
        | Not_found ->
          (
            Hashtbl.add
              new_detailed_metrics_int_map
              anomaly.Mawilab_admd.Anomaly.indice 
              (Detailed_metrics.new_empty_t ())
          )
      )
      anomaly_container;

    (* t.detailed_metrics_int_map <- new_detailed_metrics_int_map; *)
    
    debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: end";

    new_t
      new_detailed_metrics_int_map

  (* t *)
  )
