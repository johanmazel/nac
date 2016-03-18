
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
    match_timestamps

    (* five_tuple_flow_anomaly_indice_container_ref *)
    (* five_tuple_flow_element_anomaly_indice_container_ref *)
    five_tuple_flow_anomaly_indice_container
    five_tuple_flow_element_anomaly_indice_container

    t

    packet_parsing_mode
    packet_data_for_metrics
  =
  (
    (* debug "add_packet_ethernet: call"; *)

    (* debug "add_packet_ethernet: building five_tuple_flow"; *)
    
    let five_tuple_flow = 
      Five_tuple_flow.of_packet_data_for_metrics
        packet_data_for_metrics
    in

    (* debug "add_packet_ethernet: OK"; *)
    
    (* debug "add_packet_ethernet: five_tuple_flow: %s" (Five_tuple_flow.to_string five_tuple_flow); *)

    (* let anomaly_indice_list_ref = *)
    (*   try *)
    (*     ( *)
    (*       let anomaly_indice_list = *)
    (*         Five_tuple_flow_anomaly_indice_container_ref.find *)
    (*           five_tuple_flow_anomaly_indice_container_ref *)

    (*           (\* match_timestamps *\) *)

    (*           five_tuple_flow *)
    (*           (float_of_int packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec) *)
    (*       in *)

    (*       (\* debug *\) *)
    (*       (\*   "add_packet_ethernet: found: %s" *\) *)
    (*       (\*   (L.to_string *\) *)
    (*       (\*      ~sep: "\n" *\) *)
    (*       (\*      string_of_int *\) *)
    (*       (\*      (L.sort compare anomaly_indice_list) *\) *)
    (*       (\*   ); *\) *)

    (*       (\* debug *\) *)
    (*       (\*   "add_packet_ethernet: found in:\n%s" *\) *)
    (*       (\*   (Five_tuple_flow_anomaly_indice_container.to_string *\) *)
    (*       (\*      five_tuple_flow_anomaly_indice_container *\) *)
    (*       (\*   ); *\) *)

    (*       anomaly_indice_list *)
    (*     ) *)
    (*   with *)
    (*   | Not_found -> *)
    (*     ( *)
    (*       (\* debug "add_packet_ethernet: not_found"; *\) *)

    (*       (\* debug *\) *)
    (*       (\*   "add_packet_ethernet: not found in five_tuple_flow_anomaly_indice_container:\n%s" *\) *)
    (*       (\*   (Five_tuple_flow_anomaly_indice_container.to_string *\) *)
    (*       (\*      five_tuple_flow_anomaly_indice_container *\) *)
    (*       (\*   ); *\) *)

    (*       let anomaly_list_to_add = *)
    (*         Five_tuple_flow_element_anomaly_indice_container_ref.get_anomaly_indice_list *)
    (*           match_timestamps *)
    (*           five_tuple_flow_element_anomaly_indice_container_ref *)

    (*           five_tuple_flow *)
    (*       in *)

    (*       let anomaly_indice_list_to_add = *)
    (*         L.map *)
    (*           (fun anomaly -> *)
    (*              anomaly.Admd.Instantiation.Base.Anomaly.indice *)
    (*           ) *)
    (*           (\* (fun (indice, _, _, _) -> *\) *)
    (*           (\*    indice *\) *)
    (*           (\* ) *\) *)
    (*           anomaly_list_to_add *)
    (*       in *)

    (*       (\* debug *\) *)
    (*       (\*   "add_packet_ethernet: anomaly_indice_list to add: %s" *\) *)
    (*       (\*   (L.to_string *\) *)
    (*       (\*      ~sep: " " *\) *)
    (*       (\*      string_of_int *\) *)
    (*       (\*      (L.sort compare anomaly_indice_list_to_add) *\) *)
    (*       (\*   ); *\) *)

    (*       if L.length anomaly_indice_list_to_add = 0 then *)
    (*         [] *)
    (*       else *)
    (*         ( *)
    (*           L.iter *)
    (*             (fun anomaly -> *)
    (*                let indice = anomaly.Admd.Instantiation.Base.Anomaly.indice in *)
    (*                (\* (fun (indice, slice_l, start_time, end_time) -> *\) *)

    (*                Five_tuple_flow_anomaly_indice_container_ref.add *)
    (*                  five_tuple_flow_anomaly_indice_container_ref *)

    (*                  five_tuple_flow *)

    (*                  (float_of_int anomaly.Admd.Instantiation.Base.Anomaly.start_time) *)
    (*                  (float_of_int anomaly.Admd.Instantiation.Base.Anomaly.end_time) *)
    (*                  (\* (float_of_int start_time) *\) *)
    (*                  (\* (float_of_int end_time) *\) *)
    (*                  indice *)
    (*                ; *)
    (*             ) *)
    (*             anomaly_list_to_add; *)

    (*           (\* debug *\) *)
    (*           (\*   "add_packet_ethernet: new five_tuple_flow_anomaly_indice_container:\n%s" *\) *)
    (*           (\*   (Five_tuple_flow_anomaly_indice_container.to_string *\) *)
    (*           (\*      five_tuple_flow_anomaly_indice_container *\) *)
    (*           (\*   ); *\) *)

    (*           let anomaly_indice_list = *)
    (*             try *)
    (*               ( *)
    (*                 let anomaly_indice_list = *)
    (*                   Five_tuple_flow_anomaly_indice_container_ref.find *)
    (*                     five_tuple_flow_anomaly_indice_container_ref *)

    (*                     (\* match_timestamps *\) *)

    (*                     five_tuple_flow *)
    (*                     (float_of_int packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec) *)
    (*                 in *)

    (*                 anomaly_indice_list *)
    (*               ) *)
    (*             with *)
    (*             | Not_found -> *)
    (*               ( *)
    (*                 print_endline *)
    (*                   (sprintf *)
    (*                      "[Anomaly_five_tuple_flow_metrics_container]: add_packet_ethernet: could not find five_tuple_flow: %s !!!!!!!!!!!!!!!!!" *)
    (*                      (Five_tuple_flow.to_string five_tuple_flow) *)
    (*                   ); *)
    (*                 assert(false) *)
    (*               ) *)
    (*           in *)

    (*           anomaly_indice_list *)
    (*         ) *)
    (*     ) *)
    (* in *)

    (* debug *)
    (*   "add_packet_ethernet: anomaly_indice_list for five_tuple_flow: %s" *)
    (*   (L.to_string *)
    (*      ~sep: " " *)
    (*      string_of_int *)
    (*      (L.sort compare anomaly_indice_list) *)
    (*   ); *)

    (* L.iter *)
    (*   (fun anomaly_indice -> *)
    (*      try *)
    (*        ( *)
    (*          let five_tuple_flow_metrics_container_found = *)
    (*            HT.find *)
    (*              t.h *)
    (*              anomaly_indice *)
    (*          in *)

    (*          Five_tuple_flow_metrics_container.add_packet_ethernet *)
    (*            five_tuple_flow_metrics_container_found *)
    (*            packet_parsing_mode *)
    (*            packet_data_for_metrics; *)
    (*        ) *)
    (*      with *)
    (*      | Not_found -> *)
    (*        ( *)
    (*          let five_tuple_flow_metrics_container = *)
    (*            Five_tuple_flow_metrics_container.new_empty_t *)
    (*              0 *)
    (*          in *)
    (*          Five_tuple_flow_metrics_container.add_packet_ethernet *)
    (*            five_tuple_flow_metrics_container *)
    (*            packet_parsing_mode *)
    (*            packet_data_for_metrics; *)
    (*          HT.add *)
    (*            t.h *)
    (*            anomaly_indice *)
    (*            five_tuple_flow_metrics_container; *)

    (*        ) *)
    (*   ) *)
    (*   anomaly_indice_list; *)

    let anomaly_indice_list =
      try
        (
          let timestamp_sec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec in
          (* let timestamp_usec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_usec in *)

          (* debug "add_packet_ethernet: calling Five_tuple_flow_anomaly_indice_manager"; *)

          let anomaly_indice_list =
            Five_tuple_flow_anomaly_indice_manager.get_indice
              match_timestamps

              five_tuple_flow_anomaly_indice_container
              five_tuple_flow_element_anomaly_indice_container

              five_tuple_flow
              timestamp_sec
              0
          in

          (* debug "add_packet_ethernet: OK"; *)

          anomaly_indice_list
        )
      with
      | Not_found ->
        (* debug "add_packet_ethernet: not found with Five_tuple_flow_anomaly_indice_manager"; *)

        []
    in

    (* print_endline "test"; *)

    (* if *)
    (*   L.compare *)
    (*     compare *)
    (*     (L.sort compare anomaly_indice_list_ref) *)
    (*     (L.sort compare anomaly_indice_list) *)
    (*   <> *)
    (*   0 *)
    (* then *)
    (*   ( *)
    (*     let timestamp_sec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec in *)

    (*     Five_tuple_flow_element_anomaly_indice_container.set_debug true; *)

    (*     let _anomaly_list = *)
    (*       Five_tuple_flow_anomaly_indice_manager.get_indice *)
    (*         match_timestamps *)

    (*         five_tuple_flow_anomaly_indice_container *)
    (*         five_tuple_flow_element_anomaly_indice_container *)

    (*         five_tuple_flow *)
    (*         timestamp_sec *)
    (*         timestamp_sec *)
    (*     in *)

    (*     Five_tuple_flow_element_anomaly_indice_container.set_debug false; *)

    (*     print_endline *)
    (*       (sprintf *)
    (*          "\n\ninconsistency problem for %s %d:\nreference: %s\nfrom:\n%s\n\nfound: %sfrom:\n%s\n\n" *)
    (*          (Five_tuple_flow.to_string five_tuple_flow) *)

    (*          packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec *)

    (*          (L.to_string string_of_int anomaly_indice_list_ref) *)
    (*          (Five_tuple_flow_element_anomaly_indice_container_ref.to_string *)
    (*             five_tuple_flow_element_anomaly_indice_container_ref *)
    (*          ) *)

    (*          (L.to_string string_of_int anomaly_indice_list) *)
    (*          (Five_tuple_flow_anomaly_indice_container.to_string *)
    (*             five_tuple_flow_anomaly_indice_container *)
    (*          ) *)
    (*       ) *)
    (*     ; *)

    (*     print_endline "failing"; *)

    (*     failwith "problem" *)
    (*   ); *)

    (* print_endline "we are OK"; *)

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

    (* debug "add_packet_ethernet: end"; *)
  )
