open Protocol_conv_xml

type dvd = {
  id: int;
  media: Media.t;
  cover: string;
  titres: Titres.t;
  annee: int option;
  edition: string option;
  editeur: string;
  stars: Stars.t;
} [@@deriving protocol ~driver:(module Xml_light)]

type t = dvd list [@@deriving protocol ~driver:(module Xml_light)]
