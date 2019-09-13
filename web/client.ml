module Config = struct
  let user_agent = "ocaml-dvdfr"
end
module D = Dvdfr.Make (Config) (Api_cohttp_lwt.Cors_anywhere)

let () = Lwt.async @@ fun () ->
  let%lwt r = D.search `Barcode "3344428039042" in
  Printf.printf "%d results.\n" (List.length r);
  r |> List.iter (fun {Dvdfr.titres; cover; _} ->
    Printf.printf "%s, %s\n" titres.fr cover;
    ()
    (*
    let e = () in
    Dom.appendChild Dom_html.document##.body (e :> Dom.node Js.t)
    *)
  );
  Lwt.return ()
