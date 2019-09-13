module Config = struct
  let user_agent = "ocaml-dvdfr"
end
module D = Dvdfr.Api.Make (Config) (Api_cohttp_lwt.Cors_anywhere)

let () = Lwt.async @@ fun () ->
  match%lwt D.search `Barcode "3344428039042" with
  | Ok r ->
    Printf.printf "%d results.\n" (List.length r);
    r |> List.iter (fun {Dvdfr.Search.titres; cover; _} ->
      Printf.printf "%s, %s\n" titres.fr cover;
      ()
      (*
      let e = () in
      Dom.appendChild Dom_html.document##.body (e :> Dom.node Js.t)
      *)
    );
    Lwt.return ()
  | Error e ->
    e |> List.iter (fun {Dvdfr.Errors.typ; code; message} ->
      Printf.printf "%s: %s - %s\n" (Dvdfr.Errors.string_of_typ typ) code message
    );
    Lwt.return ()
