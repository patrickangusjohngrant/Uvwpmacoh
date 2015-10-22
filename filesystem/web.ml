(* TODO: all sorts of optimizations:
 - connection pooling
 - keepalives
*)

open Core.Std;;
open Util;;

(* Get packages from here *)
let http_server = ref "https://raw.githubusercontent.com/patrickangusjohngrant/Uvwpmacoh-packages/master";;

let url_appender x =
    match Pcre.pmatch ~rex:(Pcre.regexp "^http(s?)://") x with
    | true -> x
    | false -> !http_server ^ x;;

let cache = ref String.Map.empty;;

(* Looks for "Cache-control: max-age=<n>", that's all *)
let cache_nuker url response = (
    let open Async.Std
    in
    let open Cohttp.Response
    in
    let open Core.Std.Option
    in
    let ret =
        match
            Cohttp.Header.get
            (Cohttp.Response.headers response)
            "Cache-Control"
        with
        | Some cache_control_header -> (
            String.split ~on:',' cache_control_header |>
            List.map
                ~f:(String.strip ~drop:(fun x -> (x = ' '))) |>
            List.map
                ~f:(String.lsplit2 ~on:'=') |>
            List.filter
                ~f:(function
                    | (Some ("max-age", x)) -> true
                    | _ -> false) |>
            (function
                | [Some (_, x)] -> Some (Float.of_string x)
                | _ -> None) >>|
            fun time -> after (Core.Span.of_sec time) |>
            (fun deferred -> (
                upon deferred (
                    fun () -> (
                        log_string INFO ("Expiring " ^ url);
                        cache := (String.Map.remove !cache url)));
                deferred)))
        | None -> None
    in match ret with
    | Some x -> x
    | None -> Async.Std.return ());;

let download_to_bigstring x = (
    let open Async.Std
    in
    let open Cohttp.Response
    in
    let downloadme = url_appender x
    in
    let actually_download () = (
        log_string INFO ("Downloading " ^ downloadme);
        downloadme |>
        Uri.of_string |>
        Cohttp_async.Client.get >>=
        (fun (response, body) ->
            match response.status with
            | `OK -> (Cohttp_async.Body.to_string body) >>= (
                don't_wait_for (cache_nuker downloadme response);
                fun x -> return (Some (Bigstring.of_string x)))
            | _ -> return None)
        |>
        (fun x -> Thread_safe.block_on_async_exn (fun () -> x)))
    in
    match String.Map.find !cache downloadme with
    | Some contents -> contents
    | None -> (
        let ret = actually_download ()
        in (
            match ret with
            | Some x -> (
                cache := String.Map.add !cache ~key:downloadme ~data:x;
                x)
            | None -> raise (ServerSideException x))));;

let download_to_string x = Bigstring.to_string (download_to_bigstring x);;
