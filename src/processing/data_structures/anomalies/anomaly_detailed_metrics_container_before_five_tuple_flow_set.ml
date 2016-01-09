
open Printf

open Map_ext_instantiations

open Ipv4
open Tcp
open Udp
open Icmp

open Admd_functor_instantiation
open Mawilab_admd_functor_instantiation

open Key_occurrence_distribution_instantiations

open Five_tuple_flow_data_structures

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
    detailed_metrics_hashtable : (int, Detailed_metrics.t) Batteries.Hashtbl.t;
    anomaly_metric_hashtable : (int, Anomaly_metric.t) Batteries.Hashtbl.t;
  }

let new_t
    detailed_metrics_hashtable
    anomaly_metric_hashtable
    =
  {
    detailed_metrics_hashtable;
    anomaly_metric_hashtable;
  }

let to_string to_string_mode t =
  match to_string_mode with
  | To_string_mode.Command ->
    Utils_batteries.to_string_hashtbl
      ~sep_element: "\n\n"
      ~sep_key_value: ": "
      ~to_string_key: (fun key -> sprintf "%d" key)
      (fun detailed_metrics -> Detailed_metrics.to_string to_string_mode detailed_metrics)
      t.detailed_metrics_hashtable
  | To_string_mode.Simple ->
    Utils_batteries.to_string_hashtbl
      ~sep_element: "\n\n"
      ~sep_key_value: ": "
      ~to_string_key: (fun key -> sprintf "%d" key)
      (fun detailed_metrics -> Detailed_metrics.to_string to_string_mode detailed_metrics)
      t.detailed_metrics_hashtable
  | To_string_mode.Normal ->
    Utils_batteries.to_string_hashtbl
      ~sep_element: "\n\n"
      ~sep_key_value: ": "
      ~to_string_key: (fun key -> sprintf "%d" key)
      (fun detailed_metrics -> Detailed_metrics.to_string to_string_mode detailed_metrics)
      t.detailed_metrics_hashtable

let length t = Hashtbl.length t.detailed_metrics_hashtable

let find_detailed_metrics indice t = Hashtbl.find t.detailed_metrics_hashtable indice 
let find_anomaly_metric indice t = Hashtbl.find t.anomaly_metric_hashtable indice 

let fold_detailed_metrics f t acc = Hashtbl.fold f t.detailed_metrics_hashtable acc
let fold_anomaly_metric f t acc = Hashtbl.fold f t.anomaly_metric_hashtable acc

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

    let nb_five_tuple_flow = Five_tuple_flow_metrics_container.length five_tuple_flow_metrics_container in
    let nb_anomaly = Mawilab_admd.Anomaly_container.length anomaly_container in

    (* let fusion_map hashtable_1 hashtable_2 = *)
    (*   debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: fusion_map: call"; *)

    (*   let detailed_metrics_int_map_1 = *)
    (*     Hashtbl.fold *)
    (*       (fun int (five_tuple_flow_list, detailed_metrics) int_map -> *)
    (*         Int_map.add *)
    (*           int *)
    (*           (five_tuple_flow_list, detailed_metrics) *)
    (*           int_map *)
    (*       ) *)
    (*       hashtable_1 *)
    (*       Int_map.empty *)
    (*   in *)

    (*   let detailed_metrics_int_map_2 = *)
    (*     Hashtbl.fold *)
    (*       (fun int (five_tuple_flow_list, detailed_metrics) int_map -> *)
    (*         Int_map.add *)
    (*           int *)
    (*           (five_tuple_flow_list, detailed_metrics) *)
    (*           int_map *)
    (*       ) *)
    (*       hashtable_2 *)
    (*       Int_map.empty *)
    (*   in *)

    (*   let int_map = *)
    (*     Int_map.merge *)
    (*       (fun int option_1 option_2 -> *)
    (*         match option_1 with *)
    (*         | None -> *)
    (*           ( *)
    (*             match option_2 with *)
    (*             | None -> failwith "Anomaly_detailed_metrics_container: apply_anomaly_container_five_tuple_flow_detailed_metrics_container: nothing a five_tuple_flow" *)
    (*             | Some (five_tuple_flow_list_2, detailed_metrics_2) -> Some (five_tuple_flow_list_2, detailed_metrics_2) *)
    (*           ) *)
    (*         | Some (five_tuple_flow_list_1, detailed_metrics_1) -> *)
    (*           ( *)
    (*             match option_2 with *)
    (*             | None -> Some (five_tuple_flow_list_1, detailed_metrics_1) *)
    (*             | Some (five_tuple_flow_list_2, detailed_metrics_2) -> *)
    (*       Some *)
    (*         ( *)
    (*           (Batteries.List.unique_cmp ~cmp: Five_tuple_flow.compare  *)
    (*        (List.append five_tuple_flow_list_1 five_tuple_flow_list_2) *)
    (*           ) *)
    (*       , *)
    (*           (Detailed_metrics.fusion detailed_metrics_1 detailed_metrics_2) *)
    (*         )  *)
    (*           ) *)
    (*       ) *)
    (*       detailed_metrics_int_map_1 *)
    (*       detailed_metrics_int_map_2 *)
    (*   in *)

    (*   let new_hashtable = Hashtbl.create (Int_map.cardinal int_map) in *)

    (*   Int_map.iter *)
    (*     (fun indice detailed_metrics -> *)
    (*       Hashtbl.add *)
    (*         new_hashtable *)
    (*         indice *)
    (*         detailed_metrics *)
    (*     ) *)
    (*     int_map; *)

    (*   debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: fusion_map: end"; *)

    (*   new_hashtable *)
    (* in *)

    let fusion_five_tuple_list_detailed_metrics_hashtable hashtable_1 hashtable_2 =
      Fusion_hashtable.fusion_int_hashtable
  (* (fun (five_tuple_flow_list_1, detailed_metrics_1) (five_tuple_flow_list_2, detailed_metrics_2) -> *)
  (fun (five_tuple_flow_set_1, detailed_metrics_1) (five_tuple_flow_set_2, detailed_metrics_2) ->
    (
      (* (Batteries.List.unique_cmp ~cmp: Five_tuple_flow.compare  *)
      (*    (Batteries.List.append five_tuple_flow_list_1 five_tuple_flow_list_2) *)
      (* ) *)
      (
        (* let set1 = Five_tuple_flow_set.of_list five_tuple_flow_list_1 in *)
        (* let set2 = Five_tuple_flow_set.of_list five_tuple_flow_list_2 in *)
        (* let union = Five_tuple_flow_set.union set1 set2 in *)
        let union = Five_tuple_flow_set.union five_tuple_flow_set_1 five_tuple_flow_set_2 in
(* Five_tuple_flow_set.to_list union *)
        union
      )
        ,
      (Detailed_metrics.fusion detailed_metrics_1 detailed_metrics_2))
  )
  hashtable_1 
  hashtable_2
    in

    let fusion_anomaly_count_hashtable hashtable_1 hashtable_2 =
      Fusion_hashtable.fusion
  (fun count_1 count_2 ->  count_1 + count_2)
  hashtable_1 
  hashtable_2
    in

    let fusion_map (count_hashtable_1, tuple_hashtable_1) (count_hashtable_2, tuple_hashtable_2) =
      let count_hashtable = fusion_anomaly_count_hashtable count_hashtable_1 count_hashtable_2 in
      let tuple_hashtable = fusion_five_tuple_list_detailed_metrics_hashtable tuple_hashtable_1 tuple_hashtable_2 in
      (count_hashtable, tuple_hashtable)
    in

    let parallelization_mode_to_use =
      if Five_tuple_flow_metrics_container.length five_tuple_flow_metrics_container < 5000000 then
  parallelization_mode
      else 
  Parallelization_mode.No_parallelization 2000
    in

    let five_tuple_flow_count_hashtable, five_tuple_flow_list_detailed_metrics_tuple_hashtable =
      Map_data.fold_list
        parallelization_mode_to_use
        (fun (count_hashtable_1, tuple_hashtable_1) (five_tuple_flow, five_tuple_flow_metrics) ->
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

            let (new_count_hashtable_1, new_tuple_hashtable_1)  =
        Mawilab_admd.Anomaly_container.fold_left
                (fun (count_hashtable_2, tuple_hashtable_2) anomaly ->
            (
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

        let count_hashtable, tuple_hashtable =
          if compare_result then
      (
                    let indice = anomaly.Mawilab_admd.Anomaly.indice in
        
        (* debug *)
        (*   "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: %d" *)
        (*   indice; *)

        let new_count_hashtable =
                            try
            (
        let anomaly_count =
                Batteries.Hashtbl.find
                                    count_hashtable_2
                                    five_tuple_flow
        in

        Batteries.Hashtbl.replace
                                  count_hashtable_2
                                  five_tuple_flow
          (anomaly_count + 1);

        count_hashtable_2
            )
                            with
                            | Not_found ->
            (
        Batteries.Hashtbl.add
          count_hashtable_2
          five_tuple_flow
          1;

        count_hashtable_2
            )
        in

                          let new_tuple_hashtable =
                            try
            (
        let five_tuple_flow_set, found_detailed_metrics =
                Batteries.Hashtbl.find
                                    tuple_hashtable_2
                                    indice
        in

        let detailed_metrics =
                                  Detailed_metrics.of_five_tuple_flow_metrics
                                    five_tuple_flow
                                    five_tuple_flow_metrics
        in
        Detailed_metrics.append
          found_detailed_metrics
          detailed_metrics;

        Hashtbl.replace
                                  tuple_hashtable_2
                                  indice
                                  (* ((five_tuple_flow :: five_tuple_flow_list), found_detailed_metrics) *)
                                  ((Five_tuple_flow_set.add five_tuple_flow five_tuple_flow_set), found_detailed_metrics)
        ;


        tuple_hashtable_2
            )
                            with
                            | Not_found ->
            (
        let new_detailed_metrics =
                Detailed_metrics.of_five_tuple_flow_metrics
                  five_tuple_flow
                  five_tuple_flow_metrics
        in

        Batteries.Hashtbl.add
                                  tuple_hashtable_2
                                  indice
                                  (Five_tuple_flow_set.singleton five_tuple_flow, new_detailed_metrics);
        
        tuple_hashtable_2
            )
                          in


        (new_count_hashtable, new_tuple_hashtable)
      )
          else
      (
                          (count_hashtable_2, tuple_hashtable_2)
      )
        in

        (count_hashtable, tuple_hashtable)
                  )
    )
                (count_hashtable_1, tuple_hashtable_1)
                anomaly_container
      in

      (new_count_hashtable_1, new_tuple_hashtable_1)
          )
        )
        fusion_map
  ((Batteries.Hashtbl.create nb_five_tuple_flow), (Batteries.Hashtbl.create nb_anomaly))
        five_tuple_flow_metrics_tuple_list
    in

    (* Completing metrics for anomalies with empty traffic *)
    Mawilab_admd.Anomaly_container.iter
      (fun anomaly ->
        try 
          (
      let _detailed_metrics_found =
        Hashtbl.find
                five_tuple_flow_list_detailed_metrics_tuple_hashtable 
                anomaly.Mawilab_admd.Anomaly.indice
      in

      ()
          )
        with
        | Not_found ->
          (
      Hashtbl.add
        five_tuple_flow_list_detailed_metrics_tuple_hashtable
        anomaly.Mawilab_admd.Anomaly.indice 
        (Five_tuple_flow_set.empty, Detailed_metrics.new_empty_t ())
          )
      )
      anomaly_container;

    (* t.detailed_metrics_int_map <- new_detailed_metrics_int_map; *)
    
    let detailed_metrics_hashtable =
      Batteries.Hashtbl.map
  (fun _ (_, detailed_metrics) -> detailed_metrics)
  five_tuple_flow_list_detailed_metrics_tuple_hashtable
    in
    
    let anomaly_metric_hashtable =
      Batteries.Hashtbl.map
  (fun indice (five_tuple_flow_list, _) ->
    let flow_number = List.length five_tuple_flow_list in

    let packet_number, byte_number =
      List.fold_left
        (fun (packet_number_acc, byte_number_acc) five_tuple_flow ->
    let five_tuple_flow_metrics =
      Five_tuple_flow_metrics_container.find
        five_tuple_flow_metrics_container
        five_tuple_flow
    in
    
    let anomaly_number_for_five_tuple_flow =
        Hashtbl.find
          five_tuple_flow_count_hashtable
          five_tuple_flow
    in

    let packet_number = 
      float_of_int five_tuple_flow_metrics.Five_tuple_flow_metrics.nb_packets 
      /.
        float_of_int anomaly_number_for_five_tuple_flow
    in
    let byte_number =
      float_of_int five_tuple_flow_metrics.Five_tuple_flow_metrics.nb_bytes
      /.
        float_of_int anomaly_number_for_five_tuple_flow
    in

    (packet_number_acc +. packet_number, byte_number_acc +. byte_number)
        )
        (0., 0.)
        five_tuple_flow_list
    in

    Anomaly_metric.new_t
      (float_of_int flow_number)
      packet_number
      byte_number
  )
  five_tuple_flow_set_detailed_metrics_tuple_hashtable
    in

    debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: end";
    
    new_t
      detailed_metrics_hashtable
      anomaly_metric_hashtable

  (* t *)
  )

(* let of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_hashtable *)
(*     parallelization_mode *)
(*     five_tuple_flow_detailed_metrics_container *)
(*     anomaly_container *)
(*     = *)
(*   ( *)
(*     debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_hashtbl: call"; *)

(*     let five_tuple_flow_metrics_tuple_list = *)
(*       Five_tuple_flow_metrics_container.to_list *)
(*         five_tuple_flow_detailed_metrics_container *)
(*     in *)

(*     let nb_anomaly = Mawilab_admd.Anomaly_container.length anomaly_container in *)

(*     let fusion_hashtable hashtable_1 hashtable_2 = *)
(*       Hashtbl.iter *)
(*   (fun indice_2 detailed_metrics_2 -> *)
(*     ( *)
(*       try( *)
(*         let detailed_metrics_1 = *)
(*     Hashtbl.find *)
(*       hashtable_1 *)
(*                   indice_2 *)
(*         in    *)

(*         Hashtbl.replace *)
(*     hashtable_1 *)
(*     indice_2 *)
(*     (Detailed_metrics.fusion detailed_metrics_1 detailed_metrics_2) *)
(*       ); *)
(*       with *)
(*       | Not_found -> *)
(*         ( *)
(*     Hashtbl.add *)
(*       hashtable_1 *)
(*       indice_2 *)
(*                   detailed_metrics_2; *)
(*         ); *)
(*     ) *)
(*   ) *)
(*   hashtable_2; *)
      
(*       hashtable_1 *)
(*     in *)

(*     let new_detailed_metrics_int_map = *)
(*       Map_data.fold_list *)
(*         parallelization_mode *)
(*         (fun hashtable_1 (five_tuple_flow, five_tuple_flow_metrics) -> *)
(*           ( *)
(*             (\* let src_ip_option *\) *)
(*       (\*     , dst_ip_option *\) *)
(*       (\*       , proto_option *\) *)
(*       (\*         , src_port_option *\) *)
(*       (\*         , dst_port_option *\) *)
(*             (\*             = *\) *)
(*             (\*   Five_tuple_flow.to_five_tuple_option *\) *)
(*             (\*     five_tuple_flow *\) *)
(*             (\* in *\) *)
(*             (\* let proto_option = *\) *)
(*       (\*   match proto_option with *\) *)
(*       (\*   | None -> None *\) *)
(*       (\*   | Some int -> Some (Transport_protocol.of_int int) *\) *)
(*       (\* in *\) *)

(*             let src_addr, *)
(*         dst_addr, *)
(*         proto, *)
(*         src_port, *)
(*         dst_port *)
(*               = *)
(*               Five_tuple_flow.to_five_tuple *)
(*                 five_tuple_flow *)
(*             in *)

(*             let new_hashtable_1 = *)
(*               Mawilab_admd.Anomaly_container.fold_left *)
(*                 (fun hashtable_2 anomaly -> *)
(*             ( *)
(*               (\* let _compare_result = *\) *)
(*               (\*   Mawilab_admd.Anomaly.match_flow_option *\) *)
(*               (\*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_sec_start *\) *)
(*               (\*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_usec_start *\) *)
(*               (\*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_sec_end *\) *)
(*               (\*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.timestamp_usec_end *\) *)
(*               (\*     five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.nb_packets *\) *)
(*               (\*     src_ip_option *\) *)
(*               (\*     dst_ip_option *\) *)
(*               (\*     proto_option *\) *)
(*               (\*     src_port_option *\) *)
(*               (\*     dst_port_option *\) *)
(*               (\*     anomaly *\) *)
(*         (\* in *\) *)
              
(*               let compare_result = *)
(*                 Mawilab_admd.Anomaly.match_flow *)
(*                   five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_start *)
(*                   five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_start *)
(*                   five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_end *)
(*                   five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_end *)
(*                         false *)
(*                   (\* five_tuple_flow_metrics.Five_tuple_flow_metrics.nb_packets *\) *)
(*                   src_addr *)
(*                   dst_addr *)
(*                   (Transport_protocol_translation.transport_protocol_for_metrics_to_admd_transport_protocol proto) *)
(*                   src_port *)
(*                   dst_port *)
(*                   anomaly *)
(*         in *)

(*               if compare_result then *)
(*                 ( *)
(*                   let indice = anomaly.Mawilab_admd.Anomaly.indice in *)
      
(*                         let new_hashtbl = *)
(*                           try *)
(*                             ( *)
(*             let found_detailed_metrics = *)
(*               Hashtbl.find *)
(*                                   hashtable_2 *)
(*                                   indice *)
(*             in *)

(*                               let detailed_metrics = *)
(*                                 Detailed_metrics.of_five_tuple_flow_metrics *)
(*                                   five_tuple_flow *)
(*                                   five_tuple_flow_metrics *)
(*                               in *)

(*                               let updated_detailed_metrics = *)
(*                                 Detailed_metrics.fusion *)
(*                                   found_detailed_metrics *)
(*                                   detailed_metrics *)
(*                               in *)

(*                               Hashtbl.replace *)
(*                                 hashtable_2 *)
(*                                 indice *)
(*                                 updated_detailed_metrics; *)

(*                               (\* BUG: netscanUDNet become unknown *\) *)
(*                               (\* Detailed_metrics.update_five_tuple_flow_detailed_metrics *\) *)
(*                               (\*   found_detailed_metrics *\) *)
(*                               (\*   five_tuple_flow *\) *)
(*                               (\*   five_tuple_flow_detailed_metrics; *\) *)

(*                               hashtable_2 *)
(*                             ) *)
(*                           with *)
(*                           | Not_found -> *)
(*                             ( *)
(*             let new_detailed_metrics = *)
(*               Detailed_metrics.of_five_tuple_flow_metrics *)
(*                 five_tuple_flow *)
(*                 five_tuple_flow_metrics *)
(*             in *)

(*                               Hashtbl.add *)
(*                                 hashtable_2 *)
(*                                 indice *)
(*                                 new_detailed_metrics; *)
            
(*             hashtable_2 *)
(*                             ) *)
(*                         in *)

(*                         new_hashtbl *)
(*                 ) *)
(*                     else *)
(*                       ( *)
(*                         hashtable_2 *)
(*                       ) *)
(*             ) *)
(*                 ) *)
(*                 hashtable_1 *)
(*                 anomaly_container *)
(*             in *)

(*             new_hashtable_1 *)
(*           ) *)
(*         ) *)
(*         fusion_hashtable *)
(*         (Hashtbl.create nb_anomaly) *)
(*         five_tuple_flow_metrics_tuple_list *)
(*     in *)

(*     (\* Completing metrics for anomalies with empty traffic *\) *)
(*     Mawilab_admd.Anomaly_container.iter *)
(*       (fun anomaly -> *)
(*         try  *)
(*           ( *)
(*             let _detailed_metrics_found = *)
(*               Hashtbl.find *)
(*                 new_detailed_metrics_int_map  *)
(*                 anomaly.Mawilab_admd.Anomaly.indice *)
(*             in *)

(*             () *)
(*           ) *)
(*         with *)
(*         | Not_found -> *)
(*           ( *)
(*             Hashtbl.add *)
(*               new_detailed_metrics_int_map *)
(*               anomaly.Mawilab_admd.Anomaly.indice  *)
(*               (Detailed_metrics.new_empty_t ()) *)
(*           ) *)
(*       ) *)
(*       anomaly_container; *)

(*     (\* t.detailed_metrics_int_map <- new_detailed_metrics_int_map; *\) *)
    
(*     debug "of_anomaly_container_five_tuple_flow_metrics_container_parallelized_list_fusion_map: end"; *)

(*     new_t *)
(*       new_detailed_metrics_int_map *)

(*   (\* t *\) *)
(*   ) *)
