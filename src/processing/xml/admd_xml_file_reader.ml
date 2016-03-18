
module L = BatList

open Map_ext_instantiations

let process
    parallelization_mode

    xml_file_path
  =
  let mawilab_admd_file =
    Admd.Instantiation.Base.File.of_filename
      false

      parallelization_mode

      xml_file_path
  in

  let anomaly_container = mawilab_admd_file.Admd.Instantiation.Base.File.anomaly_container in

  (
    mawilab_admd_file,
    anomaly_container
  )
