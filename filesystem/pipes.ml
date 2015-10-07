open Core.Std;;

type message_to_fuse =
    | DaemonStarted of string * Pid.t
    | DaemonDied of string
    | ResolvConf of string
    | InitReady;;

type message_to_init =
    | DaemonEnable of string * ((string * string) list) * string list
    | Spawn of string list
    | PackagesComplete
    | FuseComplete;;

let close_us = ref [];;

let pipes () =
let (reader_fd, writer_fd) = Unix.pipe ()
in
let (reader_chan, writer_chan) = (
    Unix.in_channel_of_descr reader_fd,
    Unix.out_channel_of_descr writer_fd
) in (
    close_us := reader_fd :: writer_fd :: !close_us;
    (fun () -> Marshal.from_channel reader_chan),
    fun x -> Marshal.to_channel writer_chan x []; flush writer_chan
);;

let (init_reader:(unit -> message_to_init)),
    (init_writer:(message_to_init -> unit))
        = pipes ();;
let (fuse_reader:(unit -> message_to_fuse)),
    (fuse_writer:(message_to_fuse -> unit))
        = pipes ();;
