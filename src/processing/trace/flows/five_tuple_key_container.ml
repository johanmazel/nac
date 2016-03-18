
open Printf

module A = BatArray
module S = BatString
module HT = BatHashtbl
module L = BatList

open Set_ext_instantiations
(* open Ipaddr_sets *)

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

module Admd_ipaddr_set = struct
  include Batteries.Set.Make(Admd.Ipaddr_sb)

  let of_list = L.fold_left (fun acc x -> add x acc) empty
  let to_list = elements

  let fold_succ t f acc =
    if is_empty t then
      acc
    else
      let set = t in

      let set_without_first =
  remove
    (min_elt set)
    set
      in

      let previous_ref = ref (min_elt set) in

      let result =
  fold
    (fun current acc ->
     let new_acc =
       f
         !previous_ref
         current
         acc
     in

     previous_ref := current;

     new_acc
    )
    set_without_first
    acc
      in

      result

  let to_string
      ?sep: (sep = " ")
      t
      =
    let list = elements t in
    
    List_ext.to_string
      ~sep
       Admd.Ipaddr_sb.to_string
      list
end

type t =
  {
    src_addr_set : Admd_ipaddr_set.t;
    dst_addr_set : Admd_ipaddr_set.t;
    admd_transport_protocol_set :  Admd.Transport_protocol_data_structures.Set.t;
    src_port_set : Int_set.t;
    dst_port_set : Int_set.t;
  }

let new_t
    src_addr_set
    dst_addr_set
    admd_transport_protocol_set
    src_port_set
    dst_port_set
  =
  {
    src_addr_set;
    dst_addr_set;
    admd_transport_protocol_set;
    src_port_set;
    dst_port_set;
  }

(* let new_empty_t *)
(*     () *)
(*     = *)
(*   new_t *)
(*     (Hashtbl.create 0) *)
(*     (Hashtbl.create 0) *)
(*     (Hashtbl.create 0) *)
(*     (Hashtbl.create 0) *)
(*     (Hashtbl.create 0) *)

let of_filter_criteria_list filter_criteria_list =
  (
    let src_addr_set =
      Admd_ipaddr_set.of_list
        (L.filter_map
           (fun filter_criteria ->
              match filter_criteria with
              |  Admd.Filter_criteria.Src_ip data -> Some data
              | _ -> None
           )
           filter_criteria_list
        )
    in

    let dst_addr_set =
      Admd_ipaddr_set.of_list
        (L.filter_map
           (fun filter_criteria ->
              match filter_criteria with
              |  Admd.Filter_criteria.Dst_ip data -> Some data
              | _ -> None
           )
           filter_criteria_list
        )
    in

    let admd_transport_protocol_set =
       Admd.Transport_protocol_data_structures.Set.of_list
        (L.filter_map
           (fun filter_criteria ->
              match filter_criteria with
              |  Admd.Filter_criteria.Transport_protocol data -> Some data
              | _ -> None
           )
           filter_criteria_list
        )
    in

    let src_port_set =
      Int_set.of_list
        (L.filter_map
           (fun filter_criteria ->
              match filter_criteria with
              |  Admd.Filter_criteria.Src_port data -> Some data
              | _ -> None
           )
           filter_criteria_list
        )
    in

    let dst_port_set =
      Int_set.of_list
        (L.filter_map
           (fun filter_criteria ->
              match filter_criteria with
              |  Admd.Filter_criteria.Dst_port data -> Some data
              | _ -> None
           )
           filter_criteria_list
        )
    in

    new_t
      src_addr_set
      dst_addr_set
      admd_transport_protocol_set
      src_port_set
      dst_port_set
  )

let mem_src_addr
      src_addr
      t
  =
  (* Int32_set.mem *)
  Admd_ipaddr_set.mem
    src_addr
    t.src_addr_set
    
let mem_dst_addr
      dst_addr
      t
  =
  (* Int32_set.mem *)
  Admd_ipaddr_set.mem
    dst_addr
    t.dst_addr_set

let mem_admd_transport_protocol
    admd_transport_protocol
    t
    =
   Admd.Transport_protocol_data_structures.Set.mem
    admd_transport_protocol
    t.admd_transport_protocol_set

let mem_src_port
    src_port
    t
    =
  Int_set.mem
    src_port
    t.src_port_set

let mem_dst_port
    dst_port
    t
    =
  Int_set.mem
    dst_port
    t.dst_port_set

let mem_any five_tuple_flow t =
  (* let src_addr = Ipaddr.V4.to_int32 five_tuple_flow.Five_tuple_flow.src_addr in *)
  (* let dst_addr = Ipaddr.V4.to_int32 five_tuple_flow.Five_tuple_flow.dst_addr in *)
  (* let src_addr = Ipaddr.V4 five_tuple_flow.Five_tuple_flow.src_addr in *)
  (* let dst_addr = Ipaddr.V4 five_tuple_flow.Five_tuple_flow.dst_addr in *)
  let src_addr = five_tuple_flow.Five_tuple_flow.src_addr in
  let dst_addr = five_tuple_flow.Five_tuple_flow.dst_addr in
  let admd_transport_protocol = 
    Transport_protocol_translation.transport_protocol_for_metrics_to_admd_transport_protocol
      five_tuple_flow.Five_tuple_flow.protocol
  in
  let src_port = five_tuple_flow.Five_tuple_flow.src_port in
  let dst_port = five_tuple_flow.Five_tuple_flow.dst_port in

  mem_src_addr ( Admd.Ipaddr_sb.of_ipaddr src_addr) t
  ||
  mem_dst_addr ( Admd.Ipaddr_sb.of_ipaddr dst_addr) t
  ||
  mem_admd_transport_protocol admd_transport_protocol t
  ||
  mem_src_port src_port t
  ||
  mem_dst_port dst_port t
      
