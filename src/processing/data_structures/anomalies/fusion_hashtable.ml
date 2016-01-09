
open Printf

open Map_ext_instantiations

let fusion data_fusion hashtable_1 hashtable_2 =
  let new_hashtable =
    Hashtbl.copy
      hashtable_1
  in

  Hashtbl.iter
    (fun key data ->
      (
  try(
    let data_found =
      Hashtbl.find
        new_hashtable
        key
    in
    
    Hashtbl.replace
      new_hashtable
      key
      (data_fusion data_found data)
  );
  with
  | Not_found ->
    (
      Hashtbl.add
        new_hashtable
        key
        data;
    );
      )
    )
    hashtable_2;
  
  new_hashtable
    
let fusion_int_hashtable data_fusion hashtable_1 hashtable_2 =
  let int_map_1 =
    Hashtbl.fold
      (fun int data int_map ->
        Int_map.add
          int
          data
          int_map
      )
      hashtable_1
      Int_map.empty
  in

  let int_map_2 =
    Hashtbl.fold
      (fun int data int_map ->
        Int_map.add
          int
          data
          int_map
      )
      hashtable_2
      Int_map.empty
  in

  let int_map =
    Int_map.merge
      (fun int option_1 option_2 ->
        match option_1 with
        | None ->
          (
            match option_2 with
            | None -> failwith "Fusion_hashtable: fusion_int_hashtable: nothing a five_tuple_flow"
            | Some data -> Some data
          )
        | Some data_1 ->
          (
            match option_2 with
            | None -> Some data_1
            | Some data_2 ->
        Some (data_fusion data_1 data_2)
          )
      )
      int_map_1
      int_map_2
  in

  let new_hashtable = Hashtbl.create (Int_map.cardinal int_map) in

  Int_map.iter
    (fun indice data ->
      Hashtbl.add
        new_hashtable
        indice
        data
    )
    int_map;

  new_hashtable
