
open Printf

open Traffic_flow_aggr_data_instantiations.Traffic_flow_five_tuple_flow_detailed_metrics_aggr_data

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
  (fun s -> Format.printf "[Five_tuple_flow_metrics_container]: %s@." s)
      else
  ignore
    )
    fmt

type t =
  {
    container : Simple_key_data_container.t;
  }

let new_t
    container
    =
  {
    container = container;
  }

let new_empty_t
    size
    =
  let container_empty =
    Simple_key_data_container.new_empty_t
      size
  in

  new_t
    container_empty

let to_string t =
  Simple_key_data_container.to_string
    t.container

let length t = Simple_key_data_container.length t.container

let iter f t =
  Simple_key_data_container.iter
    f
    t.container

let fold f t acc_init  =
  let new_acc =
    Simple_key_data_container.fold
      f
      t.container
      acc_init
  in

  new_acc

let to_list t = Simple_key_data_container.to_list t.container

let filteri f t =
  let new_container =
    Simple_key_data_container.filteri
      f
      t.container
  in
  new_t
    new_container

let add_packet_ethernet
    t
    packet_parsing_mode
    (* pcap_header *)
    (* (ethernet_pdu : Ethernet.Ethernet.o) *)
    packet_data_for_metrics
  =
  (
    (* debug "add_packet: call"; *)

    (* let timestamp = pcap_header.Pcap_types.ts.Pcap_types.tv_sec in *)

    (* TODO: use usec too *)
    (* Pcap.tv_usec = timestamp_usec *)

    (* print_endline                    *)
    (*   (sprintf                       *)
    (*       "add_packet: timestamp %d" *)
    (*       timestamp                  *)
    (*   );                             *)

    (* let packet_data_for_metrics = *)
    (*   Packet_data_for_metrics.of_melange_ethernet *)
    (*   packet_parsing_mode *)
    (*   pcap_header *)
    (*   ethernet_pdu *)
    (* in *)

    try
      (
        let five_tuple_flow = 
          Five_tuple_flow.of_packet_data_for_metrics
            packet_data_for_metrics
        in

        try
          (
            let five_tuple_flow_metrics_found =
              Simple_key_data_container.find
                t.container
                five_tuple_flow
            in

            Five_tuple_flow_metrics.update
              five_tuple_flow_metrics_found
              packet_data_for_metrics;

            (* Simple_key_data_container.replace *)
            (*   t.container *)
            (*   five_tuple_flow *)
            (*   data_found *)
          )
        with
        | Not_found ->
          (
            (* let new_five_tuple_flow_detailed_metrics = *)
            (*   Five_tuple_flow_detailed_metrics.new_empty_t *)
            (*     () *)
            (* in *)

            (* let packet_timestamp_sec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec in *)
            (* let packet_timestamp_usec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_usec in *)

            (* let new_five_tuple_flow_detailed_metrics = *)
            (*   Five_tuple_flow_detailed_metrics.new_empty_t *)
            (*     () *)
            (* in *)

            (* Five_tuple_flow_detailed_metrics.update *)
            (*   new_five_tuple_flow_detailed_metrics *)
            (*   packet_data_for_metrics; *)

            let new_five_tuple_flow_metrics =
              Five_tuple_flow_metrics.of_packet_data_for_metrics
                packet_data_for_metrics
            in

            (* if *)
            (*   new_five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.nb_packets *)
            (*   <> *)
            (*     _new_five_tuple_flow_detailed_metrics.Five_tuple_flow_detailed_metrics.nb_packets *)
            (* then *)
            (*   ( *)
            (*     exit 50 *)
            (*   ); *)

            (* if Five_tuple_flow_detailed_metrics.compare new_detailed_metrics new_t new_five_tuple_flow_detailed_metrics <> 0 then *)
            (*   exit 30; *)

            (* let transport_layer_metrics = *)
            (*   Transport_layer_metrics.of_packet_data_for_metrics *)
            (*     packet_data_for_metrics *)
            (* in *)

            (* let new_ = *)
            (*   Five_tuple_flow_detailed_metrics.new_empty_t *)
            (*     transport_layer_metrics *)
            (* in *)

            (* let new_key_simple_data = *)
            (*   Key_simple_data.new_t *)
            (*     five_tuple_flow *)
            (*     new_detailed_metrics *)
            (* in *)

            Simple_key_data_container.add
              t.container
              five_tuple_flow
              new_five_tuple_flow_metrics
          );
      )
    with
    | Five_tuple_flow.Not_IP ->
      ()
      (* debug "add_packet: end"; *)
  )

let verify_timestamps
    t
  =
  Simple_key_data_container.iter
    (fun five_tuple_flow five_tuple_flow_detailed_metrics ->
       let timestamp_sec_start = five_tuple_flow_detailed_metrics.Five_tuple_flow_metrics.timestamp_sec_start in
       let timestamp_usec_start = five_tuple_flow_detailed_metrics.Five_tuple_flow_metrics.timestamp_usec_start in
       let timestamp_sec_end = five_tuple_flow_detailed_metrics.Five_tuple_flow_metrics.timestamp_sec_end in
       let timestamp_usec_end = five_tuple_flow_detailed_metrics.Five_tuple_flow_metrics.timestamp_usec_end in
       let nb_packets = five_tuple_flow_detailed_metrics.Five_tuple_flow_metrics.nb_packets in

       let time_stamp_error () =
         print_endline
           (sprintf
              "Five_tuple_flow_metrics_container: of_: flow start is not before flow end: %d.%d >= %d.%d"
              timestamp_sec_start
              timestamp_usec_start
              timestamp_sec_end
              timestamp_usec_end
           );
         assert(false);
       in

       if timestamp_sec_start > timestamp_sec_end then
         time_stamp_error ();

       if timestamp_sec_start = timestamp_sec_end then
         (
           if
             (timestamp_usec_start > timestamp_usec_end)
             ||            
             (timestamp_usec_start = timestamp_usec_end && nb_packets > 1)
           then
             time_stamp_error ()     
         );
    )
    t.container

let verify
    t
  =
  Simple_key_data_container.iter
    (fun five_tuple_flow five_tuple_flow_metrics ->
       Five_tuple_flow_metrics.verify
         (Five_tuple_flow_metrics.to_string
            five_tuple_flow_metrics
         )
         five_tuple_flow_metrics;
    )
    t.container

let map_to_hashtbl
    f
    t
  =
  Simple_key_data_container.map_to_hashtbl
    f
    t.container

let find
    t
    five_tuple_flow
  =
  Simple_key_data_container.find
    t.container
    five_tuple_flow
