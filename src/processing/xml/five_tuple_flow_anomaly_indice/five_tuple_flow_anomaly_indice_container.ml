
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
        (fun s -> Format.printf "[Five_tuple_flow_anomaly_indice_container]: %s@." s)
      else
        ignore
    )
    fmt

(* TODO: split in two HT: 5tp -> int_set & int -> itv-tree because add would only modify a single itv-tree (the one for the considered anomaly indice) *)
type t =
  {
    h : (Five_tuple_flow.t, ((float * float * int) list * int Interval_tree.t)) HT.t;
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
       (fun (five_tuple_flow, (l, it)) ->
          sprintf
            "%s (%d elements):\n%s"
            (Five_tuple_flow.to_string five_tuple_flow)
            (L.length l)
            (L.to_string
               (fun (start_time, end_time, indice) -> sprintf "%f %f: %d" start_time end_time indice)
               l
            )
       )
       l
    )
    
(* let to_string t = *)
(*   sprintf *)
(*     "Five_tuple_flow_anomaly_indice_container:\nsrc_ipaddr_anomaly_h:\n%s\n\ndst_ipaddr_anomaly_h:\n%s\nsrc_port_anomaly_h:\n%s\n\n%s\n\ndst_port_anomaly_h:\n%s" *)
(*     (L.to_string *)
(*        ~sep: "\n" *)
(*        (fun (ipaddr, anomaly_indice_l) -> *)
(*           sprintf *)
(*             "%s: %s" *)
(*             (Ipaddr.to_string ipaddr) *)
(*             (\* (L.to_string string_of_int anomaly_indice_l) *\) *)
(*             (Int_set.to_string anomaly_indice_l) *)
(*        ) *)
(*        (L.of_enum (HT.enum t.src_ipaddr_anomaly_h)) *)
(*     ) *)
(*     (L.to_string *)
(*        ~sep: "\n" *)
(*        (fun (ipaddr, anomaly_indice_l) -> *)
(*           sprintf *)
(*             "%s: %s" *)
(*             (Ipaddr.to_string ipaddr) *)
(*             (\* (L.to_string string_of_int anomaly_indice_l) *\) *)
(*             (Int_set.to_string anomaly_indice_l) *)
(*        ) *)
(*        (L.of_enum (HT.enum t.dst_ipaddr_anomaly_h)) *)
(*     ) *)

(*     (L.to_string *)
(*        ~sep: "\n" *)
(*        (fun (port, anomaly_indice_l) -> *)
(*           sprintf *)
(*             "%d: %s" *)
(*             port *)
(*             (\* (L.to_string string_of_int anomaly_indice_l) *\) *)
(*             (Int_set.to_string anomaly_indice_l) *)
(*        ) *)
(*        (L.of_enum (HT.enum t.src_port_anomaly_h)) *)
(*     ) *)
(*     (L.to_string *)
(*        ~sep: "\n" *)
(*        (fun (port, anomaly_indice_l) -> *)
(*           sprintf *)
(*             "%d: %s" *)
(*             port *)
(*             (\* (L.to_string string_of_int anomaly_indice_l) *\) *)
(*             (Int_set.to_string anomaly_indice_l) *)
(*        ) *)
(*        (L.of_enum (HT.enum t.dst_port_anomaly_h)) *)
(*     ) *)

(*     (L.to_string *)
(*        ~sep: "\n" *)
(*        (fun (indice, anomaly) -> *)
(*           sprintf *)
(*             "%d:\n%s" *)
(*             indice *)
(*             (Base.Anomaly.to_string *)
(*                anomaly *)
(*             ) *)
(*        ) *)
(*        (L.of_enum (HT.enum t.anomaly_h)) *)
(*     ) *)

let find
    t

    match_timestamps

    five_tuple_flow
    timestamp
  =
  let l, tree = HT.find t.h five_tuple_flow in

  if match_timestamps then
    let interval_l =
      Interval_tree.query
        tree
        timestamp
    in  

    L.map
      (fun interval ->
         (* let _, _, indice = interval.Interval_tree.Interval.to_triplet interval in *)
         (* indice *)
         interval.Interval_tree.Interval.value
      )
      interval_l
  else
    L.map
      (fun (_, _, i) ->
         i
      )
      l

let add
    t

    five_tuple_flow

    timestamp_start
    timestamp_end
    anomaly_indice
  =
  try
    (
      let l, tree = HT.find t.h five_tuple_flow in
      let new_l =
        L.append
          l
          [ (timestamp_start, timestamp_end, anomaly_indice) ]
      in
      let new_tree =
        Interval_tree.of_triplets
          new_l
      in
      HT.replace
        t.h
        five_tuple_flow
        (new_l, new_tree);
    )
  with
  | Not_found ->
    (
      let l =
        [ (timestamp_start, timestamp_end, anomaly_indice) ]
      in
      let tree =
        Interval_tree.of_triplets
          l
      in

      HT.add
        t.h
        five_tuple_flow
        (l, tree);        
      ;
    )
