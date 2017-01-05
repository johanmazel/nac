
open Printf

module L = List_ext
(* module HT = BatHashtbl *)

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Five_tuple_flow_anomaly_indice_manager]: %s@." s)
      else
        ignore
    )
    fmt

(* let bootstrap *)

(*                   five_tuple_flow_element_anomaly_indice_container *)
(*   = *)

let get_indice
    match_timestamp

    five_tuple_flow_anomaly_indice_container
    five_tuple_flow_element_anomaly_indice_container

    five_tuple_flow
    timestamp_sec
    timestamp_usec
  =
  (
    (* debug "get_indice: call"; *)

    let timestamp_float =
      float_of_int timestamp_sec
      +.
      0.000001 *. float_of_int timestamp_usec
    in

    let anomaly_indice_list =
      try
        (
          let l =
            Five_tuple_flow_anomaly_indice_container.find
              five_tuple_flow_anomaly_indice_container

              match_timestamp

              five_tuple_flow
              timestamp_float
          in

          (* debug *)
          (*   "get_indice: found (length %d): %s" *)
          (*   (L.length l) *)
          (*   (L.to_string *)
          (*      ~sep: "\n" *)
          (*      string_of_int *)
          (*      (L.sort compare l) *)
          (*   ); *)

          (* debug *)
          (*   "get_indice: found in:\n%s" *)
          (*   (Five_tuple_flow_anomaly_indice_container.to_string *)
          (*      five_tuple_flow_anomaly_indice_container *)
          (*   ); *)

          l
        )
      with
      | Not_found ->
        (
          (* debug *)
          (*   "get_indice: not found in five_tuple_flow_anomaly_indice_container:\n%s" *)
          (*   (Five_tuple_flow_anomaly_indice_container.to_string *)
          (*      five_tuple_flow_anomaly_indice_container *)
          (*   ); *)

          (* debug "get_indice: getting data from five_tuple_flow_element_anomaly_indice_container"; *)

          let anomaly_data_list_to_add =
            (* Five_tuple_flow_element_anomaly_indice_container.get_anomaly_data_list_for_timestamp_five_tuple_flow           *)
            (*   five_tuple_flow_element_anomaly_indice_container *)

            (*   match_timestamp *)

            (*   timestamp_sec *)
            (*   timestamp_usec *)
            (*   timestamp_sec *)
            (*   timestamp_usec *)

            (*   five_tuple_flow *)
            Five_tuple_flow_element_anomaly_indice_container.get_anomaly_data_list_for_five_tuple_flow          
              five_tuple_flow_element_anomaly_indice_container

              (* match_timestamp *)

              (* timestamp_sec *)
              (* timestamp_usec *)
              (* timestamp_sec *)
              (* timestamp_usec *)

              five_tuple_flow
          in

          (* debug "get_indice: mapping l"; *)

          let anomaly_indice_list_to_add =
            L.map
              (fun (indice, _, _, _, _, _) ->
                 indice
              )
              anomaly_data_list_to_add
          in

          (* debug *)
          (*   "get_indice: anomaly_indice_list to add: %s" *)
          (*   (L.to_string *)
          (*      ~sep: " " *)
          (*      string_of_int *)
          (*      (L.sort compare anomaly_indice_list_to_add) *)
          (*   ); *)

          let l_result =
            if L.length anomaly_indice_list_to_add = 0 then
              []
            else
              (
                L.iter
                  (fun (indice, slice_l, start_sec, start_usec, stop_sec, stop_usec) ->
                     (* let indice = anomaly.Admd.Instantiation.Base.Anomaly.indice in *)

                     let start =
                       float_of_int start_sec
                       +.
                       0.000001 *. float_of_int start_usec
                     in
                     let stop =
                       float_of_int stop_sec
                       +.
                       0.000001 *. float_of_int stop_usec
                     in

                     Five_tuple_flow_anomaly_indice_container.add
                       five_tuple_flow_anomaly_indice_container

                       five_tuple_flow

                       (* (float_of_int anomaly.Admd.Instantiation.Base.Anomaly.start_time) *)
                       (* (float_of_int anomaly.Admd.Instantiation.Base.Anomaly.end_time) *)
                       start
                       stop
                       indice
                     ;
                  )
                  anomaly_data_list_to_add;

                (* debug *)
                (*   "get_indice: new five_tuple_flow_anomaly_indice_container:\n%s" *)
                (*   (Five_tuple_flow_anomaly_indice_container.to_string *)
                (*      five_tuple_flow_anomaly_indice_container *)
                (*   ); *)

                (* debug "get_indice: getting new l"; *)

                let anomaly_indice_list =
                  try
                    (
                      let anomaly_indice_list =
                        Five_tuple_flow_anomaly_indice_container.find
                          five_tuple_flow_anomaly_indice_container

                          match_timestamp

                          five_tuple_flow
                          timestamp_float
                      in

                      anomaly_indice_list
                    )
                  with
                  | Not_found ->
                    (
                      print_endline
                        (sprintf
                           "[Five_tuple_flow_anomaly_indice_manager]: get_indice: could not find five_tuple_flow: %s at %d.%d !!!!!!!!!!!!!!!!!"
                           (Five_tuple_flow.to_string five_tuple_flow)
                           timestamp_sec
                           timestamp_usec
                        );
                      (* TODO: check why not stop for assert(fasle) in callbackinterval *)
                      assert(false)
                    )
                in

                (* debug *)
                (*   "get_indice: new found (length %d): %s" *)
                (*   (L.length anomaly_indice_list) *)
                (*   (L.to_string *)
                (*      ~sep: "\n" *)
                (*      string_of_int *)
                (*      (L.sort compare anomaly_indice_list) *)
                (*   ); *)

                anomaly_indice_list
              )
          in

          (* debug "get_indice: returning l"; *)

          l_result 

          (* assert(false); *)
        )
    in

    let r =
      if L.length anomaly_indice_list = 0 then
        raise Not_found
      else
        anomaly_indice_list
    in

    (* debug "get_indice: end"; *)

    r
  )
