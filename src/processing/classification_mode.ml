
open Printf

type t =
| Not_defined
| Trace of string
| Xml of Xml_attribute_building_mode.t

let to_string t =
  match t with
  | Not_defined -> "Not_defined"
  | Trace string -> sprintf "Trace on %s" string
  | Xml xml_attribute_building_mode -> sprintf "Xml on %s" (Xml_attribute_building_mode.to_string xml_attribute_building_mode)
