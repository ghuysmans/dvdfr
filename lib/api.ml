open Protocol_conv_xml

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
      Lwt.return (Error (Errors.of_xml_light_exn xml))
    with Failure _ ->
      match Search.of_xml_light xml with
      | Ok l -> Lwt.return (Ok l)
      | Error e ->
        Lwt.fail_with @@ "Dvdfr.search: " ^ Xml_light.error_to_string_hum e
end
