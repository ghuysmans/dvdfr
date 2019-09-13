module Config = struct
  let user_agent = "ocaml-dvdfr"
end
module D = Dvdfr.Make (Config) (Cohttp_lwt_unix.Client)
open Dvdfr

let () = Lwt_main.run @@
  let t, q =
    match Sys.argv with
    | [| _; "-b"; bc |] -> `Barcode, bc
    | [| _; t |] -> `Title, t
    | _ ->
      Printf.eprintf "usage: %s [-b] query\n" Sys.argv.(0);
      exit 1
  in
  let%lwt r = D.search ~with_actors:true t q in
  match r with
  | Ok l ->
    l |> List.iter (fun {titres; cover; stars; _} ->
      Printf.printf "%s, %s\n" titres.fr cover;
      stars |> List.iter @@ fun {typ; nom; _} ->
        Printf.printf "\t%s : %s\n" (string_of_typ typ) nom
    );
    Lwt.return ()
  | Error e ->
    e |> List.iter (fun {etyp; code; message} ->
      Printf.printf "%s: %s - %s\n" (string_of_etyp etyp) code message
    );
    Lwt.return ()
