type t =
  | DVD
  | BRD
  | HDDVD
  | HD
  | UMD
  | BRD_3D
  | UHD

let is_3d = function
  | BRD_3D -> true
  | _ -> false

let to_string = function
  | DVD -> "DVD"
  | BRD -> "BRD"
  | HDDVD -> "HDDVD"
  | HD -> "HD"
  | UMD -> "UMD"
  | BRD_3D -> "BRD-3D"
  | UHD -> "UHD"

let of_string = function
  | "DVD" -> DVD
  | "BRD" -> BRD
  | "HDDVD" -> HDDVD
  | "HD" -> HD
  | "UMD" -> UMD
  | "BRD-3D" -> BRD_3D
  | "UHD" -> UHD
  | s -> failwith @@ "Dvdfr.Media.of_string: " ^ s

let to_xml_light m =
  Xml.Element ("media", [], [Xml.PCData (to_string m)])

let of_xml_light_exn = function
  | Xml.(Element ("media", _, [PCData m])) -> of_string m
  | _ -> failwith "Dvdfr.Media.of_xml_light_exn"
