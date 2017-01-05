
open Printf

module L = List_ext
module A = Array_ext
module HT = BatHashtbl

open Set_ext_instantiations
    
let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Five_tuple_flow_timestamp_anomaly_indice_container]: %s@." s)
      else
        ignore
    )
    fmt

type t =
  {
    (* h : (Five_tuple_flow.t, int Interval_tree.t) HT.t; *)
    (* h : (Five_tuple_flow.t, ((float * float * int) list * int Interval_tree.t)) HT.t; *)
    (* h : (Five_tuple_flow.t, (int array * int Interval_tree.t)) HT.t; *)
    h : (Five_tuple_flow.t, int Interval_tree.t) HT.t;
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

let length t = HT.length t.h
    
(* let to_string t = *)
(*   let l = L.of_enum (HT.enum t.h) in *)
(*   (\* let l = *\) *)
(*   (\*   L.sort *\) *)
(*   (\*     (fun (ftp1, _) (ftp2, _) -> *\) *)
(*   (\*        Five_tuple_flow.compare ftp1 ftp2 *\) *)
(*   (\*     ) *\) *)
(*   (\*     l *\) *)
(*   (\* in *\) *)

(*   sprintf *)
(*     "five_tuple_flow_timestamp_indice_container (%d):\n%s" *)
(*     (L.length l) *)
(*     (L.to_string *)
(*        ~sep: "\n\n" *)
(*        (\* (fun (five_tuple_flow, (s, it)) -> *\) *)
(*        (\*    sprintf *\) *)
(*        (\*      "%s (%d elements):\n%s" *\) *)
(*        (\*      (Five_tuple_flow.to_string five_tuple_flow) *\) *)
(*        (\*      (Int_set.cardinal s) *\) *)
(*        (\*      (Int_set.to_string *\) *)
(*        (\*         (\\* (fun (start_time, end_time, indice) -> sprintf "%f %f: %d" start_time end_time indice) *\\) *\) *)
(*        (\*         s *\) *)
(*        (\*      ) *\) *)
(*        (\* ) *\) *)
(*        (fun (five_tuple_flow, (l, it)) -> *)
(*           sprintf *)
(*             "%s (%d elements):\n%s" *)
(*             (Five_tuple_flow.to_string five_tuple_flow) *)
(*             (L.length l) *)
(*             (L.to_string *)
(*                (fun (start_time, end_time, indice) -> sprintf "%f %f: %d" start_time end_time indice) *)
(*                l *)
(*             ) *)
(*        ) *)
(*        l *)
(*     ) *)


let of_data
    parallelization_mode
    match_timestamp

    five_tuple_flow_timestamp_container
    five_tuple_flow_element_anomaly_indice_container
  =
  (
    debug "of_data: call";

    let data_l =
      Five_tuple_flow_timestamp_container.to_list
        five_tuple_flow_timestamp_container
    in
    debug
      "of_data: data_l length: %d"
      (L.length data_l)
    ;

    let five_tuple_flow_anomaly_data_l_l =
      (* L.map *)
      Map_data.map_list
        parallelization_mode
        (fun (five_tuple_flow, timestamp_l) ->
           let anomaly_data_l =
             Five_tuple_flow_element_anomaly_indice_container.get_anomaly_data_list_for_five_tuple_flow
               five_tuple_flow_element_anomaly_indice_container

               five_tuple_flow
           in

           five_tuple_flow,
           L.unique
             ~eq: (fun (i1, _, _, _, _, _) (i2, _, _, _, _, _)->
                 compare i1 i2 = 0
               )
             (* (L.flatten *)
             (*    anomaly_data_l_l *)
             (* ) *)
             anomaly_data_l
        )
        data_l
    in

    debug
      "of_data: five_tuple_flow_anomaly_data_l_l length: %d"
      (L.length five_tuple_flow_anomaly_data_l_l)
    ;

    debug "of_data: removing five tuple flow without timestamps";    
    let five_tuple_flow_anomaly_data_l_l_filtered =
      L.filter
        (fun (_, l) -> L.length l > 0)
        five_tuple_flow_anomaly_data_l_l
    in

    debug
      "of_data: five_tuple_flow_anomaly_data_l_l_filtered length: %d"
      (L.length five_tuple_flow_anomaly_data_l_l_filtered)
    ;

    debug "of_data: building interval trees";    
    let five_tuple_flow_array_tree_l =
      L.map
        (fun (five_tuple_flow, l) ->
           let data_l =
             L.map
               (fun (i, _, start_sec, start_usec, stop_sec, stop_usec) ->
                  let start = float_of_int start_sec +. (float_of_int start_usec *. 0.000001) in
                  let stop = float_of_int stop_sec +. (float_of_int stop_usec *. 0.000001) in
                  
                  if start >= stop then
                    (
                      print_endline
                        (sprintf
                           "of_data: problem: start (%f) >= stop (%f): %d"
                           start
                           stop
                           i
                        );

                      assert(false)
                    );
                  
                  start, stop, i
               )
               l
           in

           (* L.iter *)
           (*   (fun (start, stop, i) -> *)
           (*      assert(start < stop); *)
           (*   ) *)
           (*   data_l; *)

           five_tuple_flow,
           (
             (* (A.of_list *)
             (*    (L.map *)
             (*       (fun (start, stop, i) -> i) *)
             (*       data_l *)
             (*    ) *)
             (* ) *)
             (* , *)
             Interval_tree.of_triplets
               data_l
           )
        )
        five_tuple_flow_anomaly_data_l_l_filtered
    in

    let data_h =
      HT.of_enum
        (L.enum
           five_tuple_flow_array_tree_l
        )
    in

    debug "of_data: end";

    new_t
      data_h
  )

let find
    t

    match_timestamp

    five_tuple_flow
    timestamp
  =
  (* debug "find: call"; *)

  (* debug "find: five_tuple_flow: %s at %f" (Five_tuple_flow.to_string five_tuple_flow) timestamp; *)

  (* let a, tree = HT.find t.h five_tuple_flow in *)
  let tree = HT.find t.h five_tuple_flow in

  (* debug *)
  (*   "find: s: %s" *)
  (*   (\* (Int_set.to_string s) *\) *)
  (*   (L.to_string *)
  (*      (fun (start_time, end_time, indice) -> sprintf "%f %f: %d" start_time end_time indice) *)
  (*      l *)
  (*   ) *)
  (* ; *)

  if match_timestamp then
    let interval_l =
      Interval_tree.query
        tree
        timestamp
    in  

    (* debug "find: interval_l length: %d" (L.length interval_l); *)

    (* debug "find: end"; *)

    L.map
      (fun interval ->
         (* let _, _, indice = interval.Interval_tree.Interval.to_triplet interval in *)
         (* indice *)
         interval.Interval_tree.Interval.value
      )
      interval_l
  else
    (* A.to_list *)
    (*   a *)
    let l =
      L.map
        (fun (_, _, i) -> i)
        (Interval_tree.to_triplets
           tree
        )
    in
    let s = Core_kernel.Core_int.Set.of_list l in
    assert(L.length l = Core_kernel.Core_int.Set.length s);
    l
