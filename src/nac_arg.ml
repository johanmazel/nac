
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

  let anomaly_classification_parameters =
    Anomaly_classification_parameters.new_empty_t ()
  in

  let trace_name_ref = ref "" in
  let xml_name_ref = ref "" in

  let mawilab_xml_anomalous_suspicious_name_ref = ref "" in
  let mawilab_xml_notice_name_ref = ref "" in

  let options_list =
    [
      "-ct", Arg.String
        (fun value ->
           anomaly_classification_parameters.Anomaly_classification_parameters.classification_mode <-
             Classification_mode.Trace value
        ),
      "<trace> : classification on trace mode";

      "-ctx",
      Arg.Tuple
        [
          Arg.Set_string trace_name_ref;
          Arg.Set_string xml_name_ref;
          Arg.Unit
            (fun _ ->
               anomaly_classification_parameters.Anomaly_classification_parameters.classification_mode <- 
                 Classification_mode.Xml 
                   (Xml_attribute_building_mode.Trace_xml (!trace_name_ref, !xml_name_ref))
            );
        ]
      , "<trace> <xml> : classification on trace and xml mode";

      "-ctmx",
      Arg.Tuple
        [
          Arg.Set_string trace_name_ref;
          Arg.Set_string mawilab_xml_anomalous_suspicious_name_ref;
          Arg.Set_string mawilab_xml_notice_name_ref;
          Arg.Unit
            (fun _ ->
               anomaly_classification_parameters.Anomaly_classification_parameters.classification_mode <- 
                 Classification_mode.Xml 
                   (Xml_attribute_building_mode.Trace_mawilab_xml
                      (!trace_name_ref,
                       !mawilab_xml_anomalous_suspicious_name_ref,
                       !mawilab_xml_notice_name_ref
                      )
                   )
            );
        ]
      , "<trace> <xml_anomalous_suspicious> <xml_notice> : classification on trace and mawilab xml mode";

      "-cx", Arg.String
        (fun value ->
           anomaly_classification_parameters.Anomaly_classification_parameters.classification_mode <- 
             Classification_mode.Xml 
               (Xml_attribute_building_mode.Xml value)
        ), 
      "<xml> : classification on xml mode (NOT WORKING)";

      "-taxonomy", Arg.String (fun value -> (anomaly_classification_parameters.Anomaly_classification_parameters.taxonomy_filepath <- value)), "<anomaly_taxonomy file path>";

      "-ppm", Arg.String (fun value -> (anomaly_classification_parameters.Anomaly_classification_parameters.packet_parsing_mode <- Packet_parsing_mode.of_string value)), "<IP version to parse>";

      "-das", Arg.Unit
        (fun _ ->
           (anomaly_classification_parameters.Anomaly_classification_parameters.export_metrics_attributes <- true)
        )
      , ": activate detailed anomaly statistics export";

      "-p", Arg.Int
        (fun value ->
           (anomaly_classification_parameters.Anomaly_classification_parameters.parallelization_mode <- Parallelization_mode.Parmap (value, 10))
        )
      , ": parallelization on cores with parmap";
    ]
  in

  let display_usage () =
    "Usage: nac\n"
  in

  let parse_options () =
    let empty_fun parameter = print_endline (sprintf "mac: Useless parameter: %s" parameter) in

    Arg.parse 
      options_list
      empty_fun 
      (display_usage ())
  in

  print_endline "nac: Parsing parameters";
  parse_options ();

  print_endline
    (sprintf
       "nac: parameters:\n%s"
       (Anomaly_classification_parameters.to_string
          anomaly_classification_parameters
       )
    );

  ignore(
    match anomaly_classification_parameters.Anomaly_classification_parameters.classification_mode with
    | Classification_mode.Not_defined ->
      failwith "Classification mode not defined"
    | Classification_mode.Xml xml_attribute_building_mode ->
      (
        Xml_classifier.process
          anomaly_classification_parameters.Anomaly_classification_parameters.parallelization_mode

          anomaly_classification_parameters.Anomaly_classification_parameters.taxonomy_filepath

          anomaly_classification_parameters.Anomaly_classification_parameters.packet_parsing_mode

          anomaly_classification_parameters.Anomaly_classification_parameters.export_metrics_attributes

          xml_attribute_building_mode
        ;
      )
    | Classification_mode.Trace trace_file_path ->
      Trace_classifier.process
        anomaly_classification_parameters.Anomaly_classification_parameters.parallelization_mode

        anomaly_classification_parameters.Anomaly_classification_parameters.taxonomy_filepath

        anomaly_classification_parameters.Anomaly_classification_parameters.packet_parsing_mode

        anomaly_classification_parameters.Anomaly_classification_parameters.export_metrics_attributes

        trace_file_path
  );

  print_endline "nac: end";

  exit 0






