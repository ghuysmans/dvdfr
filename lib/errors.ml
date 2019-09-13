open Protocol_conv_xml

type e' = {
  code: string;
  message: string;
} [@@deriving protocol ~driver:(module Xml_light)]

type typ =
  | Fatal
  | Warning

let string_of_typ = function
  | Fatal -> "fatal"
  | Warning -> "warning"

let typ_of_string = function
  | "fatal" -> Fatal
  | "warning" -> Warning
  | _ -> failwith "Dvdfr.Errors.typ_of_string"

type e = {
  typ: typ;
  code: string;
  message: string;
}

let e_to_xml_light {typ; code; message} =
  match e'_to_xml_light {code; message} with
  | Xml.Element (_, _, ch) ->
    Xml.Element ("error", ["type", string_of_typ typ], ch)
  | _ -> failwith "Dvdfr.Errors.to_xml_light"

let e_of_xml_light_exn = function
  | Xml.Element ("error", ["type", t], _) as x ->
    let {code; message} : e' = e'_of_xml_light_exn x in
    {typ = typ_of_string t; code; message}
  | _ -> failwith "Dvdfr.Errors.e_of_xml_light_exn"

type t = e list [@@deriving protocol ~driver:(module Xml_light)]
