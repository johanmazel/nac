
open Printf

let _ =
  print_endline "nac: call";

  let one_MB = 1024 * 1024 in
  Gc.set {
    (Gc.get())
    with
      Gc.minor_heap_size = (4 * one_MB);
      (* Gc.space_overhead = 150; *)
      Gc.major_heap_increment = (32 * one_MB);
  };

  Printexc.record_backtrace true;

  let nac_parameters =
    Nac_parameters.new_empty_t ()
  in

  let p_flag =
    Core.Std.Command.Spec.flag
      "-p"
      (Core.Std.Command.Spec.optional Core.Std.Command.Spec.int)
      ~doc: "int Parallel processing with [int] processes"
  in  
  let process_p_flag use_int =
    match use_int with
    | Some int ->
      nac_parameters.Nac_parameters.parallelization_mode <-
        Parallelization_mode.Parmap (int, 1000)
    | None ->
      nac_parameters.Nac_parameters.parallelization_mode <-
        Parallelization_mode.No_parallelization
  in

  let ppm_flag =
    Core.Std.Command.Spec.flag
      "-ppm"
      (Core.Std.Command.Spec.optional Core.Std.Command.Spec.string)
      ~doc: "string parse packet with the following IP versin [4|6|46]"
  in  
  let process_ppm_flag use_string =
    match use_string with
    | Some string ->
      nac_parameters.Nac_parameters.packet_parsing_mode <-
        Packet_parsing_mode.of_string string
    | None ->
      nac_parameters.Nac_parameters.packet_parsing_mode <-
        Packet_parsing_mode.IPV4
  in

  let t_flag =
    Core.Std.Command.Spec.flag
      "-t"
      Core.Std.Command.Spec.no_arg
      ~doc: " use ADMD timestamps"
  in
  let process_t_flag t =
    match t with
    | true ->
      nac_parameters.Nac_parameters.match_timestamps <- true
    | false ->
      nac_parameters.Nac_parameters.match_timestamps <- false
  in
  
  let atf_flag =
    Core.Std.Command.Spec.flag
      "-atf"
      Core.Std.Command.Spec.no_arg
      ~doc: " completely analyze trace first (recommended if trace only contain anomalous traffic) (NB: cannot use with -t)"
  in
  let process_atf_flag atf =
    match atf with
    | true ->
      nac_parameters.Nac_parameters.build_all_stat <- true
    | false ->
      nac_parameters.Nac_parameters.build_all_stat <- false
  in

  let das_flag =
    Core.Std.Command.Spec.flag
      "-das"
      Core.Std.Command.Spec.no_arg
      ~doc: " export values and attributes of anomalies"
  in
  let process_das_flag das =
    match das with
    | true ->
      nac_parameters.Nac_parameters.export_values_attributes <- true
    | false ->
      nac_parameters.Nac_parameters.export_values_attributes <- false
  in

  let dcft_flag =
    Core.Std.Command.Spec.flag
      "-dcft"
      Core.Std.Command.Spec.no_arg
      ~doc: " do not check five tuple flow timestamp (i.e. do not check if five tuple flow with more than 1 packet have duration > 0)"
  in
  let process_dcft_flag dcft =
    match dcft with
    | true ->
      nac_parameters.Nac_parameters.check_five_tuple_flow_metrics_timestamp <- false
    | false ->
      nac_parameters.Nac_parameters.check_five_tuple_flow_metrics_timestamp <- true
  in

  let ct =
    Core.Std.Command.basic
      ~summary: "Classify anomaly directly from trace"
      Core.Std.Command.Spec.(
        empty    
        +> p_flag
        +> ppm_flag
        +> t_flag
        +> dcft_flag
        +> das_flag
        +> anon ("taxonomy_path" %: string)
        +> anon ("trace_path" %: string)
      )
      (fun p ppm t dcft das taxonomy_path trace_path () ->
         process_p_flag p;
         process_ppm_flag ppm;
         process_t_flag t;
         process_dcft_flag dcft;
         process_das_flag das;

         nac_parameters.Nac_parameters.taxonomy_filepath <- taxonomy_path;

         nac_parameters.Nac_parameters.classification_mode <-
           Classification_mode.Trace trace_path;
      )
  in

  let ctx =
    Core.Std.Command.basic
      ~summary: "Classify anomaly annotated in [xml_path] located in [trace_path] using [taxonomy_path] from trace"
      Core.Std.Command.Spec.(
        empty
        +> p_flag
        +> ppm_flag
        +> t_flag
        +> atf_flag
        +> dcft_flag
        +> das_flag
        +> anon ("taxonomy_path" %: string)
        +> anon ("trace_path" %: string)
        +> anon ("xml_path" %: string)
      )
      (fun p ppm t atf dcft das taxonomy_path trace_path xml_path () ->
         process_p_flag p;
         process_ppm_flag ppm;
         process_t_flag t;
         process_atf_flag atf;
         process_dcft_flag dcft;
         process_das_flag das;

         nac_parameters.Nac_parameters.taxonomy_filepath <- taxonomy_path;

         nac_parameters.Nac_parameters.classification_mode <-
           Classification_mode.Xml
             (Xml_attribute_building_mode.Trace_xml (trace_path, xml_path));
      )
  in

  let ctmx =
    Core.Std.Command.basic
      ~summary: "Classify anomaly annotated in [mawilab_xml_anomalous_suspicious_path,mawilab_xml_notice_path] located in [trace_path] using [taxonomy_path] from trace"
      Core.Std.Command.Spec.(
        empty
        +> p_flag
        +> ppm_flag
        +> t_flag
        +> atf_flag
        +> dcft_flag
        +> das_flag
        +> anon ("taxonomy_path" %: string)
        +> anon ("trace_path" %: string)
        +> anon ("mawilab_xml_anomalous_suspicious_path" %: string)
        +> anon ("mawilab_xml_notice_path" %: string)
      )
      (fun p ppm t atf dcft das taxonomy_path trace_path mawilab_xml_anomalous_suspicious_path mawilab_xml_notice_path () ->
         process_p_flag p;
         process_ppm_flag ppm;
         process_t_flag t;
         process_atf_flag atf;
         process_dcft_flag dcft;
         process_das_flag das;

         nac_parameters.Nac_parameters.taxonomy_filepath <- taxonomy_path;

         nac_parameters.Nac_parameters.classification_mode <-  
           Classification_mode.Xml
             (Xml_attribute_building_mode.Trace_mawilab_xml
                (trace_path,
                 mawilab_xml_anomalous_suspicious_path,
                 mawilab_xml_notice_path
                )
             );
      )
  in

  let command =
    Core.Std.Command.group
      ~summary: "Network anomaly classification"
      [ "ct", ct; "ctx", ctx; "ctmx", ctmx]
  in

  Core.Std.Command.run command;

  if
    Nac_parameters.check
      nac_parameters
    =
    false
  then
    (
      print_endline "nac: Invalid parameters (cf above)";
      exit 1;
    );

  print_endline
    (sprintf
       "nac: parameters:\n%s"
       (Nac_parameters.to_string
          nac_parameters
       )
    );

  ignore(
    match nac_parameters.Nac_parameters.classification_mode with
    | Classification_mode.Not_defined ->
      failwith "Classification mode not defined"
    | Classification_mode.Xml xml_attribute_building_mode ->
      (
        Xml_classifier.process
          nac_parameters.Nac_parameters.parallelization_mode

          nac_parameters.Nac_parameters.packet_parsing_mode
          nac_parameters.Nac_parameters.match_timestamps
          nac_parameters.Nac_parameters.build_all_stat
          nac_parameters.Nac_parameters.check_five_tuple_flow_metrics_timestamp          
          nac_parameters.Nac_parameters.export_values_attributes

          nac_parameters.Nac_parameters.taxonomy_filepath

          xml_attribute_building_mode
        ;
      )
    | Classification_mode.Trace trace_file_path ->
      Trace_classifier.process
        nac_parameters.Nac_parameters.parallelization_mode        
        nac_parameters.Nac_parameters.packet_parsing_mode
        nac_parameters.Nac_parameters.check_five_tuple_flow_metrics_timestamp        
        nac_parameters.Nac_parameters.export_values_attributes
        
        nac_parameters.Nac_parameters.taxonomy_filepath

        trace_file_path
  );

  print_endline "nac: end";

  exit 0






