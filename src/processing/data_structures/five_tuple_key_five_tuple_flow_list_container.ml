
open Printf

open Map_ext_instantiations

open Ipv4
open Tcp
open Udp
open Icmp

open Traffic_flow_aggr_data_instantiations.Traffic_flow_five_tuple_flow_detailed_metrics_aggr_data

open Key_occurrence_distribution_instantiations

let debug_enabled = ref false

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
    src_addr_hashtable : (Ipaddr.t, Five_tuple_flow.t list) Hashtbl.t;
    dst_addr_hashtable : (Ipaddr.t, Five_tuple_flow.t list) Hashtbl.t;
    admd_transport_protocol_hashtable : ( Admd.Transport_protocol.t , Five_tuple_flow.t list) Hashtbl.t;
    src_port_hashtable : (int, Five_tuple_flow.t list) Hashtbl.t;
    dst_port_hashtable : (int, Five_tuple_flow.t list) Hashtbl.t;
  }

let new_t
    src_addr_hashtable
    dst_addr_hashtable
    admd_transport_protocol_hashtable
    src_port_hashtable
    dst_port_hashtable
    =
  {
    src_addr_hashtable;
    dst_addr_hashtable;
    admd_transport_protocol_hashtable;
    src_port_hashtable;
    dst_port_hashtable;
  }

let new_empty_t
    ()
    =
  new_t
    (Hashtbl.create 0)
    (Hashtbl.create 0)
    (Hashtbl.create 0)
    (Hashtbl.create 0)
    (Hashtbl.create 0)

let of_five_tuple_flow_metrics_container
    five_tuple_flow_metrics_container
  =
  (
    let new_t = new_empty_t () in

    Five_tuple_flow_metrics_container.iter
      (fun five_tuple_flow _ ->
         (
           let src_addr = five_tuple_flow.Five_tuple_flow.src_addr in
           let dst_addr = five_tuple_flow.Five_tuple_flow.dst_addr in
           let admd_transport_protocol = 
             Transport_protocol_translation.transport_protocol_for_metrics_to_admd_transport_protocol
               five_tuple_flow.Five_tuple_flow.protocol
           in
           let src_port = five_tuple_flow.Five_tuple_flow.src_port in
           let dst_port = five_tuple_flow.Five_tuple_flow.dst_port in

           (* Source address *)
           ignore(
             try
               (
                 let five_tuple_flow_list_found =
                   Hashtbl.find
                     new_t.src_addr_hashtable
                     src_addr
                 in

                 Hashtbl.replace
                   new_t.src_addr_hashtable
                   src_addr
                   (Batteries.List.append [ five_tuple_flow ] five_tuple_flow_list_found)
               )
             with
             | Not_found ->
               (
                 Hashtbl.add
                   new_t.src_addr_hashtable
                   src_addr
                   [ five_tuple_flow ]
               )
           );
           (* Destination address *)
           ignore(
             try
               (
                 let five_tuple_flow_list_found =
                   Hashtbl.find
                     new_t.dst_addr_hashtable
                     dst_addr
                 in

                 Hashtbl.replace
                   new_t.dst_addr_hashtable
                   dst_addr
                   (Batteries.List.append [ five_tuple_flow ] five_tuple_flow_list_found)
               )
             with
             | Not_found ->
               (
                 Hashtbl.add
                   new_t.dst_addr_hashtable
                   dst_addr
                   [ five_tuple_flow ]
               )
           );

           (* Admd transport protocol *)
           ignore(
             try
               (
                 let five_tuple_flow_list_found =
                   Hashtbl.find
                     new_t.admd_transport_protocol_hashtable
                     admd_transport_protocol
                 in

                 Hashtbl.replace
                   new_t.admd_transport_protocol_hashtable
                   admd_transport_protocol
                   (Batteries.List.append [ five_tuple_flow ] five_tuple_flow_list_found)
               )
             with
             | Not_found ->
               (
                 Hashtbl.add
                   new_t.admd_transport_protocol_hashtable
                   admd_transport_protocol
                   [ five_tuple_flow ]
               )
           );

           (* Source port *)
           ignore(
             try
               (
                 let five_tuple_flow_list_found =
                   Hashtbl.find
                     new_t.src_port_hashtable
                     src_port
                 in

                 Hashtbl.replace
                   new_t.src_port_hashtable
                   src_port
                   (Batteries.List.append [ five_tuple_flow ] five_tuple_flow_list_found)
               )
             with
             | Not_found ->
               (
                 Hashtbl.add
                   new_t.src_port_hashtable
                   src_port
                   [ five_tuple_flow ]
               )
           );
           (* Destination port *)
           ignore(
             try
               (
                 let five_tuple_flow_list_found =
                   Hashtbl.find
                     new_t.dst_port_hashtable
                     dst_port
                 in

                 Hashtbl.replace
                   new_t.dst_port_hashtable
                   dst_port
                   (Batteries.List.append [ five_tuple_flow ] five_tuple_flow_list_found)
               )
             with
             | Not_found ->
               (
                 Hashtbl.add
                   new_t.dst_port_hashtable
                   dst_port
                   [ five_tuple_flow ]
               )
           );
         )
      )
      five_tuple_flow_metrics_container;

    new_t
  )

let of_five_tuple_key_container_and_five_tuple_flow_metrics_container
    five_tuple_key_container
    five_tuple_flow_metrics_container
  =
  (
    let new_t = new_empty_t () in

    Five_tuple_flow_metrics_container.iter
      (fun five_tuple_flow _ ->
         (
           let src_addr = five_tuple_flow.Five_tuple_flow.src_addr in
           let dst_addr = five_tuple_flow.Five_tuple_flow.dst_addr in
           let admd_transport_protocol = 
             Transport_protocol_translation.transport_protocol_for_metrics_to_admd_transport_protocol
               five_tuple_flow.Five_tuple_flow.protocol
           in
           let src_port = five_tuple_flow.Five_tuple_flow.src_port in
           let dst_port = five_tuple_flow.Five_tuple_flow.dst_port in

           if 
             (
               (
                 Five_tuple_key_container.mem_src_addr
                   ( Admd.Ipaddr_sb.of_ipaddr src_addr)
                   five_tuple_key_container
               )
               ||
               Five_tuple_key_container.mem_dst_addr ( Admd.Ipaddr_sb.of_ipaddr dst_addr) five_tuple_key_container
               ||
               (Five_tuple_key_container.mem_admd_transport_protocol
                  admd_transport_protocol
                  five_tuple_key_container
               )
               ||
               Five_tuple_key_container.mem_src_port src_port five_tuple_key_container
               ||
               Five_tuple_key_container.mem_dst_port dst_port five_tuple_key_container
             )
           then
             (
               (* Source address *)
               ignore(
                 try
                   (
                     let five_tuple_flow_list_found =
                       Hashtbl.find
                         new_t.src_addr_hashtable
                         src_addr
                     in

                     Hashtbl.replace
                       new_t.src_addr_hashtable
                       src_addr
                       (Batteries.List.append [ five_tuple_flow ] five_tuple_flow_list_found)
                   )
                 with
                 | Not_found ->
                   (
                     Hashtbl.add
                       new_t.src_addr_hashtable
                       src_addr
                       [ five_tuple_flow ]
                   )
               );
               (* Destination address *)
               ignore(
                 try
                   (
                     let five_tuple_flow_list_found =
                       Hashtbl.find
                         new_t.dst_addr_hashtable
                         dst_addr
                     in

                     Hashtbl.replace
                       new_t.dst_addr_hashtable
                       dst_addr
                       (Batteries.List.append [ five_tuple_flow ] five_tuple_flow_list_found)
                   )
                 with
                 | Not_found ->
                   (
                     Hashtbl.add
                       new_t.dst_addr_hashtable
                       dst_addr
                       [ five_tuple_flow ]
                   )
               );

               (* Admd transport protocol *)
               ignore(
                 try
                   (
                     let five_tuple_flow_list_found =
                       Hashtbl.find
                         new_t.admd_transport_protocol_hashtable
                         admd_transport_protocol
                     in

                     Hashtbl.replace
                       new_t.admd_transport_protocol_hashtable
                       admd_transport_protocol
                       (Batteries.List.append [ five_tuple_flow ] five_tuple_flow_list_found)
                   )
                 with
                 | Not_found ->
                   (
                     Hashtbl.add
                       new_t.admd_transport_protocol_hashtable
                       admd_transport_protocol
                       [ five_tuple_flow ]
                   )
               );

               (* Source port *)
               ignore(
                 try
                   (
                     let five_tuple_flow_list_found =
                       Hashtbl.find
                         new_t.src_port_hashtable
                         src_port
                     in

                     Hashtbl.replace
                       new_t.src_port_hashtable
                       src_port
                       (Batteries.List.append [ five_tuple_flow ] five_tuple_flow_list_found)
                   )
                 with
                 | Not_found ->
                   (
                     Hashtbl.add
                       new_t.src_port_hashtable
                       src_port
                       [ five_tuple_flow ]
                   )
               );
               (* Destination port *)
               ignore(
                 try
                   (
                     let five_tuple_flow_list_found =
                       Hashtbl.find
                         new_t.dst_port_hashtable
                         dst_port
                     in

                     Hashtbl.replace
                       new_t.dst_port_hashtable
                       dst_port
                       (Batteries.List.append [ five_tuple_flow ] five_tuple_flow_list_found)
                   )
                 with
                 | Not_found ->
                   (
                     Hashtbl.add
                       new_t.dst_port_hashtable
                       dst_port
                       [ five_tuple_flow ]
                   )
               );

             );

         )
      )
      five_tuple_flow_metrics_container;

    new_t
  )

let find_src_addr
    t
    src_addr
  =
  try
    Hashtbl.find
      t.src_addr_hashtable
      src_addr
  with
  | Not_found ->
    []
    
let find_dst_addr
    t
    dst_addr
  =
  try
    Hashtbl.find
      t.dst_addr_hashtable
      dst_addr
  with
  | Not_found ->
    []

let find_admd_transport_protocol
    t
    admd_transport_protocol
    =
  try
    Hashtbl.find
      t.admd_transport_protocol_hashtable
      admd_transport_protocol
  with
  | Not_found ->
    []

let find_src_port
    t
    src_port
  =
  try
    Hashtbl.find
      t.src_port_hashtable
      src_port
  with
  | Not_found ->
    []

let find_dst_port
    t
    dst_port
  =
  try
    Hashtbl.find
      t.dst_port_hashtable
      dst_port
  with
  | Not_found ->
    []
