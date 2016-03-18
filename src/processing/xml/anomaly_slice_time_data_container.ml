
open Printf

module L = List_ext
module HT = Hashtbl_ext

open Admd.Instantiation
       
type t =
  {
    h : (int, (int * Admd.Slice.t list * int * int)) HT.t;
  }

let new_t
    h
  =
  {
    h
  }

let of_anomaly_container
    anomaly_container
  =
  let l =
    L.mapi
      (fun i anomaly ->
         i,
         (
           anomaly.Base.Anomaly.indice,
           anomaly.Base.Anomaly.slice_list,
           anomaly.Base.Anomaly.start_time,
           anomaly.Base.Anomaly.end_time
         )
      )
      anomaly_container.Base.Anomaly_container.anomaly_list
  in

  new_t
    (HT.of_enum
       (L.enum
          l
       )
    )
    
let of_double_anomaly_container
    anomaly_container_1
    anomaly_container_2
  =
  let l =
    L.mapi
      (fun indice anomaly ->
         indice,
         (
           anomaly.Base.Anomaly.indice,
           anomaly.Base.Anomaly.slice_list,
           anomaly.Base.Anomaly.start_time,
           anomaly.Base.Anomaly.end_time
         )
      )
      (L.append
         anomaly_container_1.Base.Anomaly_container.anomaly_list
         anomaly_container_2.Base.Anomaly_container.anomaly_list
      )
  in

  new_t
    (HT.of_enum
       (L.enum
          l
       )
    )
    
let to_list t = L.of_enum (HT.enum t.h)

let length t = HT.length t.h

let iter f t = HT.iter f t.h

let get_admd_indice t indice =
  let admd_indice, _, _, _ =
    HT.find
      t.h
      indice
  in
  admd_indice
