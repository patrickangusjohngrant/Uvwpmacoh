open Core.Std
open Util

type daemon_mode = Init | InitLxc | Standalone;;

let init_mode = ref true;;
let resolv_conf = ref "";;

let mountpoint = ref "/fuse";;
let system_definition = ref "default.json";;
(* let system_definition = ref "x.json";; *)
(*let system_definition = ref "simplehttpserver.json";; *)

let speclist = [
(
    "--no-init",
    Arg.Clear init_mode,
    "Disable init mode"
);
(
    "--mountpoint",
    Arg.Set_string mountpoint,
    "Fuse mountpoint"
);
(
    "--http-server",
    Arg.Set_string Web.http_server,
    "Distro http server"
);
(
    "--definition",
    Arg.Set_string system_definition,
    "System definition"
);
];;

Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    "Usage? Whatever";;

print_string ((Sys.argv).(0));;
print_newline ();;

let mode = match !init_mode, Sys.argv with
    | true, [|"/sbin/init.lxc"|] -> InitLxc
    | true, [|"/sbin/init"|] -> Init
    | true, [|"/init"|] -> Init
    | true, _ -> assert false
    | false, _ -> Standalone;;

let pid_daemon_map = Pid.Table.create ();;
let daemon_pid_map = String.Table.create ();;

let start_daemon (name, envvar, argv) =
    match Hashtbl.find daemon_pid_map name with
    | Some _ -> () (* Noop, already running *)
    | None -> (
        spawn
        ~envvar:envvar
        ~close_stdinerr:true
        ~pidnotify:(
            fun pid -> (
                Hashtbl.add_exn pid_daemon_map pid name;
                Hashtbl.add_exn daemon_pid_map name pid;
                Pipes.fuse_writer (Pipes.DaemonStarted (name, pid))))
        ~wait:false
        argv);;

let ready_to_start_daemons = ref false;;
let queue_of_daemons = ref [];;
let enqueue_daemon x = queue_of_daemons := x :: !queue_of_daemons;;

let flush_daemons () = match !ready_to_start_daemons with
    | false -> ()
    | true -> (
        List.iter !queue_of_daemons start_daemon;
        queue_of_daemons := []);;

(* Listens for messages from fuse or whatever *)
let rec forever_read () = (
    (match Pipes.init_reader () with
        | Pipes.DaemonEnable (name, envvar, argv) -> (
            enqueue_daemon (name, envvar, argv);
            flush_daemons ())
        | Pipes.Spawn argv ->
            if !init_mode then spawn argv;
        | Pipes.FuseComplete -> (
            ready_to_start_daemons := true;
            flush_daemons ())
        | _ -> ());
    forever_read ());;

let rec forever_wait () = (
    match Unix.wait `Any with
        (pid, exit_or_signal) -> (
            match Hashtbl.find pid_daemon_map pid with
            | Some daemon -> (
                Hashtbl.remove pid_daemon_map pid;
                Hashtbl.remove daemon_pid_map daemon;
                Pipes.fuse_writer (Pipes.DaemonDied daemon))
            | None -> ());
    forever_wait ());;

let not_lxc f =
    match mode with
    | InitLxc -> ()
    | Init
    | Standalone -> f ()
in
try (
    (match mode with
    | Standalone -> never_returns (Fuseimpl.start !mountpoint !system_definition)
    | Init
    | InitLxc ->
        spawn ["/busybox"; "--install"; "/sbin"];
        spawn ["/sbin/hostname"; "uvwpmacoh"];
        not_lxc (fun () -> spawn ["/sbin/modprobe"; "e1000"]);
        not_lxc (fun () -> spawn ["/sbin/modprobe"; "fuse"]);
        Unix.mkdir ~perm:0o755 "/proc";
        Unix.mkdir ~perm:0o755 "/sys";
        spawn ["/bin/mount"; "proc"; "/proc"; "-t"; "proc"];
        spawn ["/bin/mount"; "sysfs"; "/sys"; "-t"; "sysfs"];
        spawn ["/bin/mount"; "udev"; "/dev"; "-t"; "devtmpfs"];
        Unix.mkdir ~perm:0o755 "/dev/pts";
        spawn ["/bin/mount"; "devpts"; "/dev/pts"; "-t"; "devpts"];

        spawn ["/bin/mount"; "tmpfs"; "/run"; "-t"; "tmpfs"];
        spawn ["/bin/mount"; "tmpfs"; "/root"; "-t"; "tmpfs"];

        spawn [
            "/lib/systemd/systemd-udevd";
            "--daemon";
            "--resolve-names=never"];

        Unix.sleep 2; (* Need to wait for network card to initialise *)
        spawn ["/sbin/udhcpc"; "-s"; "/dhcp_client"];
        spawn ["/sbin/udevadm"; "trigger"];
        spawn ["/sbin/udevadm"; "settle"];
        spawn ["/sbin/ifconfig"; "lo"; "127.0.0.1"; "netmask"; "255.0.0.0"];

        let open Unix in
        let size = (Unix.stat "/etc/resolv.conf").st_size |> Int64.to_int_exn
        in (
            let resolv_conf_fd =
                Unix.openfile
                    [Unix.O_RDONLY]
                    "/etc/resolv.conf"
            in (
                resolv_conf := String.create size;
                let bytes_read = Unix.read resolv_conf_fd !resolv_conf
                in (
                    assert (bytes_read = size);
                    Unix.close resolv_conf_fd)));
        Unix.mkdir ~perm:0o755 !mountpoint;

        (match Unix.fork () with
        | `In_the_child ->
            never_returns (Fuseimpl.start !mountpoint !system_definition)
        | `In_the_parent _ -> (
            (match Pipes.init_reader () with
            | Pipes.PackagesComplete ->
                Pipes.fuse_writer (Pipes.ResolvConf !resolv_conf);
                 Pipes.fuse_writer Pipes.InitReady
            | _ -> assert false);

            (* Mount these within the fuse... *)
            List.iter
                ["proc"; "sys"; "dev"; "run"; "root"]
                (fun x -> spawn
                    ~envvar:[("LD_LIBRARY_PATH", "/fuse/lib/i386-linux-gnu")]
                    ["/bin/mount"; "--move"; "/" ^ x; (!mountpoint) ^ "/" ^ x]);

            (* Free up some space. *SUPER SLOW* Not sure why. *)
            (* spawn [|"/fuse/usr/bin/find"; "/"; "-xdev"; "-type"; "f";
             * "-exec"; "/fuse/bin/rm"; "{}"; ";"|]; *)

            (* pivot_root style stuff. *)
            Unix.chdir !mountpoint;
            spawn ~chdir:(Some !mountpoint) ["/bin/mount"; "--move"; "."; "/"];
            Unix.chroot ".";

            (match Unix.fork () with
            | `In_the_child -> (
                ignore (Unix.Terminal_io.setsid ());

                List.iter !Pipes.close_us Unix.close;

                Unix.dup2
                    (Unix.descr_of_in_channel (open_in "/dev/tty1"))
                    Unix.stdin;
                Unix.dup2
                    (Unix.descr_of_out_channel (open_out "/dev/tty1"))
                    Unix.stdout;
                Unix.dup2
                    (Unix.descr_of_out_channel (open_out "/dev/tty1"))
                    Unix.stderr;

                never_returns (
                    Unix.exec
                        ~prog:"/bin/bash"
                        ~args:["/bin/bash"]
                        ~env:(`Extend [("HOME", "/root")])
                        ()));
            | `In_the_parent _ -> (
                ignore (Thread.create forever_read ());
                forever_wait ()))))))
with e -> (
    print_string ((Exn.to_string e) ^ "\n\n");
    print_string (Printexc.get_backtrace ());
    flush_all ();
    if !init_mode then (
        never_returns (Unix.exec ~prog:"/bin/sh" ~args:["/bin/sh"] ())));;
