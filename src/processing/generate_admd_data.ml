

let algorithm =
   Admd.Algorithm.new_t
    "Johan Mazel"
    "0.1"
    "Attributes based anomaly classification."
    ""
    ""
        
let analysis attributes_metadata_string =
   Admd.Analysis.new_t
    attributes_metadata_string
    ""
    ""
    ""
        
let dataset =
   Admd.Dataset.new_t
    "MAWI"
    ""
