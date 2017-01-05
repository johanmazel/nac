
open Printf

module L = List_ext
module HT = BatHashtbl

open Set_ext_instantiations
    
let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Five_tuple_flow_timestamp_container]: %s@." s)
      else
        ignore
    )
    fmt

(* TODO: split in two HT: 5tp -> int_set and int -> itv-tree because add would only modify a single itv-tree (the one for the considered anomaly indice) *)
type t =
  {
    h : (Five_tuple_flow.t, float list) HT.t;
  }

let new_t
    h
  =
  {
    h
  }

let new_empty_t
    size
    =
  new_t
    (HT.create size)

let to_string t =
  let l = L.of_enum (HT.enum t.h) in
  let l =
    L.sort
      (fun (ftp1, _) (ftp2, _) ->
         Five_tuple_flow.compare ftp1 ftp2
      )
      l
  in

  sprintf
    "five_tuple_flow_anomaly_indice_container (%d):\n%s"
    (L.length l)
    (L.to_string
       ~sep: "\n\n"
       (fun (five_tuple_flow, l) ->
          sprintf
            "%s (%d elements):\n%s"
            (Five_tuple_flow.to_string five_tuple_flow)
            (L.length l)
            (L.to_string
               (fun timestamp -> string_of_float timestamp)
               l
            )
       )
       l
    )

let to_list t = L.of_enum (HT.enum t.h)

let map_to_ht f t = HT.map f t.h
    
let of_trace
    packet_parsing_mode
    match_timestamps

    trace_statistics

    trace_file_path
  =
  (
    let h = HT.create 0 in

    Execution_time_measure.execute
      "[Five_tuple_flow_timestamp_container]: of_trace: adding packets"
      (fun _ ->
         let packet_processing_function pcap_header pcap_payload =
           Melange_wrapper.launch_function_on_header_ethernet
             (fun pcap_header ethernet_pdu ->
                let packet_data_for_metrics =
                  Packet_data_for_metrics.of_melange_ethernet
                    packet_parsing_mode
                    pcap_header
                    ethernet_pdu
                in

                match packet_data_for_metrics.Packet_data_for_metrics.pdu_t with
                | Packet_data_for_metrics.Other ->
                  ()
                | _ ->
                  let timestamp_sec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec in
                  let timestamp_usec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_usec in

                  let five_tuple_flow = 
                    Five_tuple_flow.of_packet_data_for_metrics
                      packet_data_for_metrics
                  in

                  let f =
                    float_of_int timestamp_sec
                    +.
                    0.000001 *. float_of_int timestamp_usec
                  in

                  try
                    (
                      let l =
                        HT.find
                          h
                          five_tuple_flow
                      in

                      HT.replace
                        h
                        five_tuple_flow
                        (f :: l)
                    )
                  with
                  | Not_found ->
                    (
                      HT.add
                        h
                        five_tuple_flow
                        [ f ]
                    )
             )
             pcap_header
             pcap_payload
         in

         Trace_handler.launch_analysis
           Cstruct_pcap_wrapper.launch_analysis
           trace_statistics
           packet_processing_function
      );

    let r : t = new_t h in
    r
  )


