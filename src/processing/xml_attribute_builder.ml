
open Printf

module HT = BatHashtbl

open Admd.Instantiation

open Map_ext_instantiations

open Traffic_flow_aggr_data_instantiations

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
  (fun s -> Format.printf "[Xml_attribute_builder]: %s@." s)
      else
  ignore
    )
    fmt

let process
    parallelization_mode

    date_format_string
    ?default_hour_minute_second
    time_format_string

    xml_file_path
  =
  (
    debug "process: call";

    debug "process: xml_file_path: %s" xml_file_path;

    debug "process: building admd_file";

    let mawilab_admd_file =
      Base.File.of_filename
        false

        date_format_string
        ?default_hour_minute_second
        time_format_string

        parallelization_mode

        xml_file_path
    in

    let analysis =
      match mawilab_admd_file.Base.File.analysis_option with
      | None ->
        (
          print_endline "Trace_xml_attribute_builder: cannot build network traffic attribute for empty analysis";
          assert(false)
        )
      | Some analysis -> analysis
    in

    let description = analysis.Admd_analysis.description in

    let description_string_list = Str.split (Str.regexp "--") description in

    if Batteries.List.length description_string_list <> 2 then
      (
        print_endline
          (sprintf
             "[Xml_attribute_builder]: description does not contain 2 elements:\n%s" 
             description
          );
        assert(false)
      );

    let feature_name_container_string_from_file = List.nth description_string_list 1 in

    let attributes_name_container_from_file =
      Feature_name_container.t_of_sexp
        (Sexplib.Sexp.of_string
           feature_name_container_string_from_file
        )
    in

    let feature_name_container = Network_traffic_attributes.generate_feature_name_container () in

    if Feature_name_container.compare feature_name_container attributes_name_container_from_file <> 0 then
      (
        (* TODO: use to_string on both *)
        print_endline
          "Trace_xml_attribute_builder: process: network attributes in xml file inconsistent with current version !!!!"
        ;
        assert(false)
      )
    else
      (
        let anomaly_list = Base.Anomaly_container.to_list mawilab_admd_file.Base.File.anomaly_container in

        let anomaly_h =
          Batteries.List.fold_left
            (fun h_acc anomaly ->
               (
                 HT.add
                   h_acc
                   anomaly.Base.Anomaly.indice
                   anomaly;

                 h_acc
               )
            )
            (HT.create 0)
            anomaly_list
        in

        let xml_file_basename = Filename.basename xml_file_path in
        let xml_file_basename_no_extension = Filename.chop_extension xml_file_basename in

        let xml_filename_split_list = Str.split (Str.regexp "_") xml_file_basename_no_extension in
        let xml_filename_split_list_without_cl = List.rev (List.tl (List.rev xml_filename_split_list)) in
        let xml_file_basename_no_extension_without_cl =
          List_ext.to_string
            ~sep: "_"
            (fun string -> string)
            xml_filename_split_list_without_cl
        in

        let mawilab_admd_mod_description_h =
          HT.map
            (fun indice anomaly ->
               let description_string =
                 match anomaly.Base.Anomaly.anomaly_description_option with
                 | None -> 
                   print_endline
                     "Xml_attribute_builder: process: empty anomaly description cannot extract network_attributes !!!!"
                   ;
                   assert(false)
                 | Some string -> string
               in

               let description =
                 Xml_classification_description.of_string
                   description_string
               in
               description
            )
            anomaly_h
        in

        (* TODO: remove xml_file_basename_no_extension_without_cl *)
        (
          xml_file_basename_no_extension_without_cl,
          mawilab_admd_file,
          mawilab_admd_mod_description_h
        )
      )
  )
