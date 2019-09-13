type typ =
  | Realisateur
  | Acteur

let typ_of_string = function
  | "Réalisateur" -> Realisateur
  | "Acteur" -> Acteur
  | _ -> failwith "Dvdfr.Stars.typ_of_string"

let string_of_typ = function
  | Realisateur -> "Réalisateur"
  | Acteur -> "Acteur"

type s = {
  typ: typ;
  id: int;
  nom: string;
}

let s_to_xml_light {typ; id; nom} =
  Xml.(Element (
    "star",
    ["type", string_of_typ typ; "id", string_of_int id],
    [PCData nom]
  ))

let s_of_xml_light_exn = function
  | Xml.(Element ("star", ["type", typ; "id", id], [PCData nom])) ->
    {typ = typ_of_string typ; id = int_of_string id; nom}
  | _ ->
    failwith "Dvdfr.Stars.s_of_xml_light_exn"

type t = s list

(* FIXME why? *)

let to_xml_light l =
  Xml.Element ("stars", [], List.map s_to_xml_light l)

let of_xml_light_exn = function
  | Xml.Element ("stars", _, l) -> List.map s_of_xml_light_exn l
  | _ -> failwith "Dvdfr.Stars.of_xml_light_exn"
