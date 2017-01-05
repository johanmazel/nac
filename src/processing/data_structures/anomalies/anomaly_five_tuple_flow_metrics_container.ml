
open Printf

module L = List_ext
module HT = Hashtbl_ext
  
open Set_ext_instantiations
    
open Traffic_flow_aggr_data_instantiations.Traffic_flow_five_tuple_flow_detailed_metrics_aggr_data

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Anomaly_five_tuple_flow_metrics_container]: %s@." s)
      else
        ignore
    )
    fmt

type t =
  {
    h : (int, Five_tuple_flow_metrics_container.t) HT.t;
  }

let new_t
    h
    =
  {
    h = h;
  }

let new_empty_t
    size
  =
  new_t
    (HT.create size)

let to_string t =
  HT.to_string
    ~sep_element: " "
    ~to_string_key: (fun indice -> string_of_int indice)
    (fun five_tuple_flow_metrics_container ->
       Five_tuple_flow_metrics_container.to_string
         five_tuple_flow_metrics_container
    )
    t.h

let find t key =
  HT.find
    t.h
    key

let length t = HT.length t.h

(* let iter f t = *)
(*   Simple_key_data_container.iter *)
(*     f *)
(*     t.container *)

(* let fold f t acc_init  = *)
(*   let new_acc = *)
(*     Simple_key_data_container.fold *)
(*       f *)
(*       t.container *)
(*       acc_init *)
(*   in *)

(*   new_acc *)

(* let to_list t = Simple_key_data_container.to_list t.container *)

(* let filteri f t = *)
(*   let new_container = *)
(*     Simple_key_data_container.filteri *)
(*       f *)
(*       t.container *)
(*   in *)
(*   new_t *)
(*     new_container *)

let map_to_hashtbl f t =
  HT.map
    f
    t.h

let add_packet_ethernet
    match_timestamp

    five_tuple_flow_timestamp_anomaly_indice_container

    t

    packet_parsing_mode
    packet_data_for_metrics
  =
  (
    (* debug "add_packet_ethernet: call"; *)

    (* debug "add_packet_ethernet: building five_tuple_flow"; *)

    ignore(
      try
        (
          let five_tuple_flow = 
            Five_tuple_flow.of_packet_data_for_metrics
              packet_data_for_metrics
          in

          (* let anomaly_indice_list = *)
          (*   try *)
          (*     ( *)
          (*       let timestamp_sec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec in *)
          (*       let timestamp_usec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_usec in *)

          (*       (\* debug "add_packet_ethernet: calling Five_tuple_flow_anomaly_indice_manager"; *\) *)

          (*       let anomaly_indice_list = *)
          (*         Five_tuple_flow_anomaly_indice_manager.get_indice *)
          (*           match_timestamp *)

          (*           five_tuple_flow_anomaly_indice_container *)
          (*           five_tuple_flow_element_anomaly_indice_container *)

          (*           five_tuple_flow *)

          (*           timestamp_sec *)
          (*           timestamp_usec *)
          (*       in *)

          (*       (\* debug "add_packet_ethernet: OK"; *\) *)

          (*       anomaly_indice_list *)
          (*     ) *)
          (*   with *)
          (*   | Not_found -> *)
          (*     (\* debug "add_packet_ethernet: not found with Five_tuple_flow_anomaly_indice_manager"; *\) *)

          (*     [] *)
          (* in *)

          let timestamp_sec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec in
          let timestamp_usec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_usec in

          let timestamp =
            float_of_int timestamp_sec
            +.
            0.000001 *. float_of_int timestamp_usec
          in

          let anomaly_indice_l =
            try 
              Five_tuple_flow_timestamp_anomaly_indice_container.find
                five_tuple_flow_timestamp_anomaly_indice_container

                match_timestamp

                five_tuple_flow
                timestamp
            with
            | Not_found -> []
          in

          (* let set_ref = Int_set.of_list anomaly_indice_list in *)
          (* let set = Int_set.of_list anomaly_indice_l in *)

          (* if Int_set.compare set_ref set <> 0 then *)
          (*   ( *)
          (*     debug *)
          (*       "add_packet_ethernet: different anomaly indice found for %s:\nref (%d): %s\n\nnew (%d): %s" *)
          (*       (Five_tuple_flow.to_string five_tuple_flow) *)
          (*       (Int_set.cardinal set_ref) *)
          (*       (Int_set.to_string set_ref) *)
          (*       (Int_set.cardinal set) *)
          (*       (Int_set.to_string set) *)
          (*     ; *)
          (*     assert(false) *)
          (*   ); *)

          L.iter
            (fun anomaly_indice ->
               try
                 (
                   let five_tuple_flow_metrics_container_found =
                     HT.find
                       t.h
                       anomaly_indice
                   in

                   Five_tuple_flow_metrics_container.add_packet_ethernet
                     five_tuple_flow_metrics_container_found

                     packet_parsing_mode
                     packet_data_for_metrics;
                 )
               with
               | Not_found ->
                 (
                   let five_tuple_flow_metrics_container =
                     Five_tuple_flow_metrics_container.new_empty_t
                       0
                   in
                   Five_tuple_flow_metrics_container.add_packet_ethernet
                     five_tuple_flow_metrics_container
                     packet_parsing_mode
                     packet_data_for_metrics;
                   HT.add
                     t.h
                     anomaly_indice
                     five_tuple_flow_metrics_container;

                 )
            )
            anomaly_indice_l;
        )
      with
      | Five_tuple_flow.Not_IP ->
        (
          ()
        );
    );

    (* debug "add_packet_ethernet: end"; *)
  )


let add_packet_ethernet_legacy
    match_timestamp

    five_tuple_flow_anomaly_indice_container
    five_tuple_flow_element_anomaly_indice_container

    t

    packet_parsing_mode
    packet_data_for_metrics
  =
  (
    (* debug "add_packet_ethernet: call"; *)

    (* debug "add_packet_ethernet: building five_tuple_flow"; *)

    ignore(
      try
        (
          let five_tuple_flow = 
            Five_tuple_flow.of_packet_data_for_metrics
              packet_data_for_metrics
          in

          let anomaly_indice_list =
            try
              (
                let timestamp_sec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec in
                let timestamp_usec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_usec in

                (* debug "add_packet_ethernet: calling Five_tuple_flow_anomaly_indice_manager"; *)

                let anomaly_indice_list =
                  Five_tuple_flow_anomaly_indice_manager.get_indice
                    match_timestamp

                    five_tuple_flow_anomaly_indice_container
                    five_tuple_flow_element_anomaly_indice_container

                    five_tuple_flow

                    timestamp_sec
                    timestamp_usec
                in

                (* debug "add_packet_ethernet: OK"; *)

                anomaly_indice_list
              )
            with
            | Not_found ->
              (* debug "add_packet_ethernet: not found with Five_tuple_flow_anomaly_indice_manager"; *)

              []
          in

          L.iter
            (fun anomaly_indice ->
               try
                 (
                   let five_tuple_flow_metrics_container_found =
                     HT.find
                       t.h
                       anomaly_indice
                   in

                   Five_tuple_flow_metrics_container.add_packet_ethernet
                     five_tuple_flow_metrics_container_found

                     packet_parsing_mode
                     packet_data_for_metrics;
                 )
               with
               | Not_found ->
                 (
                   let five_tuple_flow_metrics_container =
                     Five_tuple_flow_metrics_container.new_empty_t
                       0
                   in
                   Five_tuple_flow_metrics_container.add_packet_ethernet
                     five_tuple_flow_metrics_container
                     packet_parsing_mode
                     packet_data_for_metrics;
                   HT.add
                     t.h
                     anomaly_indice
                     five_tuple_flow_metrics_container;

                 )
            )
            anomaly_indice_list;
        )
      with
      | Five_tuple_flow.Not_IP ->
        (
          ()
        );
    );

    (* debug "add_packet_ethernet: end"; *)
  )
