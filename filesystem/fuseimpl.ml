open Core.Std;;
open Unix;;
open Util;;

let path_chunker y =
    String.split ~on:'/' y |>
    List.filter ~f:(fun x -> not (x = ""));;

let rec file_from_path_helper path_chunked current = (
    match path_chunked with
    | [] -> current
    | h :: t -> match current with
        | Filesystem.Directory (current_directory, _, _) ->
            file_from_path_helper t (current_directory#get_exn h)
        | _ -> raise Not_found
);;


let file_from_path path =
    file_from_path_helper (path_chunker path) Filesystem.root_entry
;;

let do_mkdir path mode =
    match List.rev (path_chunker path) with
        | [] -> raise (Unix_error (ENOENT, "mkdir", ""))
        | head :: tail ->
            match file_from_path_helper
                (List.rev tail)
                Filesystem.root_entry
            with
            | Filesystem.Directory (d, _, _) ->
                d#mkdir
                    head
                    (Filesystem.MutableStat
                        (ref (Filesystem.stat_factory S_DIR mode)))
                    Filesystem.UserPut
            | _ -> raise (Unix_error (ENOENT, "mkdir", ""));;

let do_getattr path =
    try
        let f = file_from_path path
        in
        match f with
        | Filesystem.Directory (_, stats, _) -> Filesystem.unbox_stat stats
        | Filesystem.File (f, stats, _) -> {
            (Filesystem.unbox_stat stats) with st_size = f#size
        }
        | Filesystem.Symlink (_, stats, _) -> Filesystem.unbox_stat stats
        | Filesystem.EphemeralFile _ -> assert false
        (* | Filesystem.Mount _ *)
    with Not_found -> raise (Unix_error (ENOENT, "lstat", path))

let do_readdir path _ =
    let f = file_from_path path
    in
    match f with
        | Filesystem.Directory (d, _, _) ->
            List.fold_left
                (d#ls)
                ~init:["."; ".."]
                ~f:(fun acc (name, _) -> name :: acc)
        | _ -> raise (Unix_error (ENOENT, "readdir", path));;

let do_mknod path mode =
    let rec install_file path_parts contents_factory filesystem put =
        match path_parts with
        | [] -> raise (Unix_error (ENOENT, "open", ""))
        | [h] -> filesystem#put h (contents_factory ()) put
        | h :: t -> (
            match filesystem#get_exn h with
            | Filesystem.Directory (d, _, _) -> (install_file t contents_factory d put)
            | _ -> raise (Unix_error (ENOENT, "open", "")))
    in
    let stat = ref (Filesystem.stat_factory S_REG mode)
    in
    let created_file = new Filesystem.ramfile_builder
        ""
        (fun () ->
            stat := { !stat with st_mtime = Unix.time () })
    in
        install_file
            (path_chunker path)
            (fun () ->
                Filesystem.File (
                    created_file,
                    Filesystem.MutableStat stat,
                    Filesystem.UserPut))
            Filesystem.root_filesystem
            Filesystem.UserPut
            ignore
;;

let open_files = Hashtbl.create ~hashable:Int.hashable ();;

(* Is this necessary? *)
let smallest_new_fd () =
    let rec smallest_new_fd_helper n =
        match Hashtbl.mem open_files n with
        | false -> n
        | true -> smallest_new_fd_helper (n + 1)
    in
        smallest_new_fd_helper 1
;;

let rec do_fopen path flags =
    match file_from_path path with
    | Filesystem.File (file, _, _) ->
        let f = smallest_new_fd ()
        in
        (match
            List.mem flags Unix.O_APPEND ||
            List.mem flags Unix.O_WRONLY ||
            List.mem flags Unix.O_RDWR
        with
            | true ->
                Hashtbl.add_exn
                    open_files
                    f
                    (fun (offset, contents) ->
                        file#write (flags, offset, contents));
                Some f
            | false -> None)
    | Filesystem.Symlink (s, _, _) -> do_fopen s flags
    | Filesystem.Directory _ -> raise (Unix_error (ENOENT, "open", path))
    | Filesystem.EphemeralFile _ -> assert false
    (* | Filesystem.Mount _ *)

let do_release path flags = Hashtbl.remove open_files;;

let do_read path buf ofs whatever =
    let rec blitmefinder internal_path =
        let f = file_from_path internal_path in
            match f with
            | Filesystem.Directory _ ->
                raise (Unix_error (ENOENT, "read", path))
            | Filesystem.Symlink (s, _, _) -> (blitmefinder s)
            | Filesystem.File (f, _, _) -> f#read
            | Filesystem.EphemeralFile _ -> assert false
(*            | Filesystem.Mount _ *)
    in
        let blitme = blitmefinder path
        in
        (
            let ofs = Int64.to_int_exn ofs in
                let len =
                    min
                        ((Bigarray.Array1.dim blitme) - ofs)
                        (Bigarray.Array1.dim buf)
                in (
                    Bigarray.Array1.blit
                        (Bigarray.Array1.sub blitme ofs len)
                        (Bigarray.Array1.sub buf 0 len);
                   len));;

let rec do_write path buf ofs whatever = (
    match file_from_path path with
    | Filesystem.Directory _ ->
        raise (Unix_error (ENOENT, "write", path))
    | Filesystem.Symlink (s, _, _) -> do_write s buf ofs whatever
    | Filesystem.File _ ->
        let f_open = Hashtbl.find_exn open_files whatever
        in
            f_open (ofs, buf);
            Bigarray.Array1.dim buf
    | Filesystem.EphemeralFile _ -> assert false
(*    | Filesystem.Mount _ *)
);;

let rec do_truncate path length =
    let f = file_from_path path in
        match f with
        | Filesystem.Directory _ ->
            raise (Unix_error (ENOENT, "truncate", path))
        | Filesystem.Symlink (s, _, _) -> do_truncate s length
        | Filesystem.File (f, _, _) -> f#truncate length
        | Filesystem.EphemeralFile _ -> assert false
(*        | Filesystem.Mount _ *)
;;

let do_readlink path =
    match file_from_path path with
    | Filesystem.Symlink (s, _, _) -> s
    | _ -> raise (Unix_error (ENOENT, "readlink", path))
;;

let do_unlink path = (
    match List.rev (path_chunker path) with
    | filename :: dirname -> (
        match
            file_from_path_helper
                (List.rev dirname)
                Filesystem.root_entry
        with
        | Filesystem.Directory (d, _, _) -> d#remove filename Filesystem.UserPut
        | _ -> assert false)
    | [] -> assert false (* rm / ?! *)
);;

let do_link path1 path2 = (
    let f1 = file_from_path path1
    in
    match List.rev (path_chunker path2) with
    | filename :: dirname -> (
        (* TODO: is this secure? *)
        match
            file_from_path_helper
                (List.rev dirname)
                Filesystem.root_entry
        with
        | Filesystem.Directory (d, _, _) ->
            d#put filename f1 Filesystem.UserPut ignore
        | _ -> assert false)
    | [] -> assert false
);;

let do_rename path1 path2 = (
    try do_unlink path2 with _ -> ();
    do_link path1 path2;
    do_unlink path1
);;

let stat_helper f = match file_from_path f with
    | Filesystem.Directory (_, stat, _) -> Filesystem.unbox_stat_ref stat
    | Filesystem.File (_, stat, _) -> Filesystem.unbox_stat_ref stat
    | Filesystem.Symlink (_, stat, _) -> Filesystem.unbox_stat_ref stat
    | Filesystem.EphemeralFile _ -> assert false
(*    | Filesystem.Mount _*)

let do_chmod name perm =
    let s = stat_helper name
    in
    s := { !s with st_perm = perm };;

let do_chown name uid gid =
    let s = stat_helper name
    in
    s := { !s with st_uid = uid; st_gid = gid };;

let do_utime name atime mtime =
    let s = stat_helper name
    in
    s := { !s with st_atime = atime; st_mtime = mtime }
;;

(* TODO: update this function to rethrow exceptions as Unix_errors *)
let log_wrapper_3 name f a1 a2 a3 =
    try f a1 a2 a3 with e -> (
        log_exception DEBUG e;
        raise e);;

let log_wrapper_2 name f a1 a2 = log_wrapper_3 name (fun x -> x) f a1 a2;;
let log_wrapper_1 name f a = log_wrapper_2 name (fun x -> x) f a;;
let my_main argv =
  Fuse.main argv (* Sys.argv *)
    {
      Fuse.default_operations with
        Fuse.getattr = log_wrapper_1 "getattr" do_getattr;
        Fuse.mknod = log_wrapper_2 "mknod" do_mknod;
        Fuse.readdir = log_wrapper_2 "readdir" do_readdir;
        Fuse.fopen = log_wrapper_2 "fopen" do_fopen;
        Fuse.read = log_wrapper_3 "read" do_read;
        Fuse.write = log_wrapper_3 "write" do_write;
        Fuse.release = log_wrapper_3 "release" do_release;
        Fuse.mkdir = log_wrapper_2 "mkdir" do_mkdir;
        Fuse.truncate = log_wrapper_2 "truncate" do_truncate;
        Fuse.readlink = log_wrapper_1 "readlink" do_readlink;
        Fuse.unlink = log_wrapper_1 "unlink" do_unlink;
        Fuse.link = log_wrapper_2 "link" do_link;
        Fuse.rename = log_wrapper_2 "rename" do_rename;
        Fuse.chmod = log_wrapper_2 "chmod" do_chmod;
        Fuse.chown = log_wrapper_3 "chown" do_chown;
        Fuse.utime = log_wrapper_3 "utime" do_utime;
    }


let start mountpoint definition = (
    Filesystem.build_filesystem definition;

    Thread.create
        Filesystem.fuse_reader_loop
        ()
    |> ignore;

    Pipes.init_writer Pipes.PackagesComplete;

    Thread.create my_main [|
            "init";
            "-f"; mountpoint;
            "-o"; "allow_other";
    |] |> ignore;

    Async.Std.Scheduler.go ()
)
