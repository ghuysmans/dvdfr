open Protocol_conv_xml

type t = {
  fr: string;
  vo: string option;
  alternatif: string option;
  alternatif_vo: string option;
} [@@deriving protocol ~driver:(module Xml_light)]
