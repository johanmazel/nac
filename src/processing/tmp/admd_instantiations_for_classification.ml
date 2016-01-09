


module Mawilab_admd_classic = Admd.Data.Make(Mawilab_anomaly_type)(Admd.Instantiation.Base_value)(Admd.Instantiation.Base_description)

module Mawilab_admd_xml = Admd.Data.Make(Mawilab_anomaly_type)(Admd.Instantiation.Base_value)(Xml_classification_description)

module Admd_mawilab_type_base_value_binary_description =
  Admd.Data.Make(Mawilab_anomaly_type)(Admd.Instantiation.Base_value)(Binary_classification_description)



