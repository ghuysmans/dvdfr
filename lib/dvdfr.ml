open Protocol_conv_xml

type titres = {
  fr: string;
  vo: string option;
  alternatif: string option;
  alternatif_vo: string option;
} [@@deriving protocol ~driver:(module Xml_light)]

type typ =
  | Realisateur
  | Acteur

let typ_of_string = function
  | "Réalisateur" -> Realisateur
  | "Acteur" -> Acteur
  | _ -> failwith "typ_of_string"

let string_of_typ = function
  | Realisateur -> "Réalisateur"
  | Acteur -> "Acteur"

type star = {
  typ: typ;
  id: int;
  nom: string;
}

let star_to_xml_light {typ; id; nom} =
  Xml.(Element (
    "star",
    ["type", string_of_typ typ; "id", string_of_int id],
    [PCData nom]
  ))

let star_of_xml_light_exn = function
  | Xml.(Element ("star", ["type", typ; "id", id], [PCData nom])) ->
    {typ = typ_of_string typ; id = int_of_string id; nom}
  | _ ->
    failwith "star_of_xml_light_exn"

type media =
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

let string_of_media = function
  | DVD -> "DVD"
  | BRD -> "BRD"
  | HDDVD -> "HDDVD"
  | HD -> "HD"
  | UMD -> "UMD"
  | BRD_3D -> "BRD-3D"
  | UHD -> "UHD"

let media_of_string = function
  | "DVD" -> DVD
  | "BRD" -> BRD
  | "HDDVD" -> HDDVD
  | "HD" -> HD
  | "UMD" -> UMD
  | "BRD-3D" -> BRD_3D
  | "UHD" -> UHD
  | s -> failwith @@ "media_of_string: " ^ s

let media_to_xml_light m =
  Xml.(Element ("media", [], [PCData (string_of_media m)]))

let media_of_xml_light_exn = function
  | Xml.(Element ("media", _, [PCData m])) -> media_of_string m
  | _ -> failwith "media_of_xml_light_exn"

type stars = star list

let stars_to_xml_light l =
  Xml.Element ("stars", [], List.map star_to_xml_light l)

let stars_of_xml_light_exn = function
  | Xml.Element ("stars", _, l) -> List.map star_of_xml_light_exn l
  | _ -> failwith "stars_of_xml_light_exn"

type dvd = {
  id: int;
  media: media;
  cover: string;
  titres: titres;
  annee: int option;
  edition: string option;
  editeur: string;
  stars: stars;
} [@@deriving protocol ~driver:(module Xml_light)]

type dvds = dvd list [@@deriving protocol ~driver:(module Xml_light)]

type error' = {
  code: string;
  message: string;
} [@@deriving protocol ~driver:(module Xml_light)]

type etyp =
  | Fatal
  | Warning

let string_of_etyp = function
  | Fatal -> "fatal"
  | Warning -> "warning"

let etyp_of_string = function
  | "fatal" -> Fatal
  | "warning" -> Warning
  | _ -> failwith "etyp_of_string"

type error = {
  etyp: etyp;
  code: string;
  message: string;
}

let error_to_xml_light {etyp; code; message} =
  match error'_to_xml_light {code; message} with
  | Xml.Element (_, _, ch) ->
    Xml.Element ("error", ["type", string_of_etyp etyp], ch)
  | PCData _ -> failwith "error_to_xml_light"

let error_of_xml_light_exn = function
  | Xml.Element ("error", ["type", t], _) as x ->
    let {code; message} : error' = error'_of_xml_light_exn x in
    {etyp = etyp_of_string t; code; message}
  | x -> failwith (Xml_light.to_string_hum x)

type errors = error list [@@deriving protocol ~driver:(module Xml_light)]


open Lwt.Infix
exception HttpError of int

module type C = sig
  val user_agent : string
end

module Make (CONFIG : C) (CLIENT : Cohttp_lwt.S.Client) = struct
  let get uri =
    CLIENT.get uri >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK ->
      Cohttp_lwt.Body.to_string body >|= fun raw ->
      let p = XmlParser.make () in
      XmlParser.parse p (XmlParser.SString raw)
    | status ->
      Lwt.fail (HttpError (Cohttp.Code.code_of_status status))

  let search t ?(with_actors=false) q =
    let uri =
      Uri.(add_query_params
        (of_string "https://www.dvdfr.com/api/search.php")
          (
            (match t with
             | `Barcode -> "gencode", [q]
             | `Title -> "title", [q]) ::
            (if with_actors then ["withActors", []] else [])
          )
      )
    in
    get uri >>= fun xml ->
    try
      Lwt.return (Error (errors_of_xml_light_exn xml))
    with Failure _ ->
      match dvds_of_xml_light xml with
      | Ok l -> Lwt.return (Ok l)
      | Error e ->
        Lwt.fail_with @@ "Dvdfr.search: " ^ Xml_light.error_to_string_hum e
end
