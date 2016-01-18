open Core.Std;;

let dev_null = "/dev/null";;

let spawn
    ?(wait = true)
    ?(chdir = None)
    ?(envvar = [("PATH", "/busybox")])
    ?(close_stdinerr = false)
    ?(pidnotify = fun _ -> ())
    args =
    match Unix.fork () with
        | `In_the_child -> (
            ignore (Unix.Terminal_io.setsid ());
            List.iter !Pipes.close_us Unix.close;
            (match close_stdinerr with
            | true ->
                Unix.dup2
                    (Unix.descr_of_in_channel (open_in dev_null))
                    Unix.stdin;
                Unix.dup2
                    (Unix.descr_of_out_channel (open_out dev_null))
                    Unix.stdout;
                Unix.dup2
                    (Unix.descr_of_out_channel (open_out dev_null))
                    Unix.stderr;
            | false -> ());
            (match chdir with
            | Some s -> (Unix.chdir s)
            | None -> ());
            never_returns
                (Unix.exec
                    ~prog:(List.hd_exn args)
                    ~args:args
                    ~env:(`Replace envvar)
                    ()))
        | `In_the_parent pid -> (
            pidnotify pid;
            (* This raises an exception if non-zero or a signal *)
            if wait then Unix.waitpid_exn pid);;


(* TODO: Make this a dequeue of finite size? *)
let logs = Buffer.create 16;;

type loglevel = NONE | DEBUG | INFO | WARNING;;

let log_to_console = ref INFO;;

(* TODO: allow console and logfile to be done independently. Also log the
severity and the time like a real logger.  *)
let log_string loglevel x =
match !log_to_console, loglevel with
    | DEBUG, _
    | INFO, INFO
    | INFO, WARNING
    | WARNING, WARNING ->
        let message = (
            (match loglevel with
            | DEBUG -> "DEBUG "
            | INFO -> "INFO "
            | WARNING -> "WARNING "
            | _ -> assert false) ^ x ^ "\n")
        in
            print_string message;
            flush_all ();
            Buffer.add_string logs message
    | _ -> ()
;;

(* TODO: look at that stuff that's new in 4.02..? *)
let log_exception loglevel e =
    log_string loglevel (
        (Exn.to_string e) ^ "\n" ^ (Printexc.get_backtrace ()));;

exception ServerSideException of string;;
