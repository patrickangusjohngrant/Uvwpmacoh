open Unix
open LargeFile
open Core.Std
open Util

(*
* TODO:
* - split into more files
* - do dependencies "right"
* - export/import system definition!!!!!
* - better exception handling (or rethrowing)
* - go through overlays and figure out what should actually be done with them
*   - stuff put online
*   - stuff evaluated locally
* - locking. EVERYWHERE.
* - ramfile configuration.
*    - disposable
*    - non-disposable
*    - log
* - stop using keys as values
*   - update in forward and reverse serialization functions
*   - update depackagator.py
*   - update system definitions
* - factor out the aggregate directories into configuration
* - move passwd.d/ etc stuff into root package
* - use LXC
* - do ramfiles (and maybe downloaded files?) through mmap'd real files.
* - add package metadata such as version, url, etc to /control/packages/*/
*)

(* Number of next free inode *)
let inode =
    let number = ref 0 in
    let next () = (incr number; !number) in next;;

(* Produces an "kind" (directory, etc) inode of "perm" (0o755, etc)
*)
let stat_factory kind perm =
    let time = Unix.time ()
    in {
        st_ino = inode ();
        st_dev = 1;
        st_kind = kind;
        st_perm = perm;
        st_nlink = 1;
        st_uid = 0;
        st_gid = 0;
        st_rdev = 0;
        st_size = 0L;
        st_atime = time;
        st_mtime = time;
        st_ctime = time};;

exception SecurityException;;

class file = object (self)
    method read : Fuse.buffer = raise (Unix_error (ENOENT, "read", ""));
    method write : (Unix.open_flag list * int64 * Fuse.buffer -> unit) =
        raise (Unix_error (ENOENT, "write", ""));
    method truncate: (int64 -> unit) =
        raise (Unix_error (ENOENT, "truncate", ""));
    method to_json: ((string * Yojson.Basic.json) option) = None;
    method size: int64 =
        Bigarray.Array1.dim self#read |> Int64.of_int
end

(* Three types of "put", of increasing importance.
   * UserPut -- initiated by the user through a system call.
   * InternalPut -- initiated through the filesystem or a package install
   * ForcePut -- initated internally (presumably triggered by one of the other
     two, and vetted)
 "puts" are used for "putting" (or removing) files around the place.
*)
type put = UserPut | InternalPut | ForcePut;;

type statholder =
    | MutableStat of Unix.stats ref
    | ImmutableStat of Unix.stats;;

(* Register a function to "undo" a put in a package install that's awry. *)
type undoer = ((unit -> unit) -> unit);;

type directory = <
    ls: (string * directoryentry) list;
    mkdir : string -> statholder -> put -> unit;
    put : string -> directoryentry -> put -> undoer -> unit;
    get : string -> directoryentry option;
    get_exn : string -> directoryentry;
    remove: string -> put -> unit; (* TODO: Add undoer *)
    contains: string -> bool;
    merge: directory -> undoer -> unit;
    child_directory: string -> directory;
    receive: Pipes.message_to_fuse -> unit;
>
and directoryentry =
    | Directory of (directory * statholder * put)
    | File of (file * statholder * put)
    | Symlink of (string * statholder * put)
    | EphemeralFile of (file -> unit)
(*    | Mount of (string * string * string option);; *)

exception MergeFail;;

let unbox_stat stat = match stat with
    | MutableStat s -> !s
    | ImmutableStat s -> s;;

let unbox_stat_ref stat = match stat with
    | MutableStat s -> s
    | ImmutableStat _ -> raise (Unix_error (EPERM, "", ""))

let deferred_functions = ref [];;

let purge_deferred () = (
    List.iter !deferred_functions (fun f -> f ());
    deferred_functions := [];
    Pipes.init_writer Pipes.FuseComplete
);;

let defer_function f = deferred_functions := (f :: !deferred_functions);;

class hash_directory =
    object (self)
        val hash = Hashtbl.create ~hashable:String.hashable ();
        method ls = Hashtbl.fold hash ~init:[]
            ~f:(fun ~key ~data c -> (key, data) :: c);
        method mkdir name stat put = (match put with
            | UserPut -> raise SecurityException
            | ForcePut | InternalPut ->
                self#put
                    name
                    (Directory (self#child_directory name, stat, put))
                    ForcePut
                    ignore);
        method remove name put = (match put with
            | UserPut -> raise SecurityException
            | ForcePut | InternalPut -> Hashtbl.remove hash name);
        method put name f p undoer =
            match p, f, self#get name with
            | ForcePut, _, _
            | InternalPut, Symlink _, _
            | InternalPut, File _, _
            | InternalPut, EphemeralFile _, None ->
                Hashtbl.add_exn hash ~key:name ~data:f;
                undoer (fun () -> Hashtbl.remove hash name)
            | InternalPut, EphemeralFile fn, Some _ ->
                (* This is used for various magic/ephemeral files which
                are applied to other files in various ways, deferred until the
                end of installs.  *)
                defer_function (
                    fun () ->
                        (fn (
                            match self#get_exn name with
                            | File (f, _, _) -> f
                            | _ -> assert false)))
            | InternalPut, Directory (_, stats, _), None -> (
                (* If we're merging in a directory, then we want to create
                a new directory using the "mkdir" rather than just dropping in
                the new one. This is because "mkdir" might have interesting
                properties that it'll inherit from the parent, or it might do
                some interesting/useful side-effects (sorry). *)
                self#mkdir name stats p;

                (* Once this is done we can continue and assume that the
                directory is already created. *)
                self#put name f p undoer)
            | InternalPut,
                Directory (d_mergee, _, p_mergee),
                Some (Directory (d_self, _, p_self)) -> (
                (* We're merging a directory with a directory! This is
                perfectly sensible. Let's do that recursively. *)
                match p_mergee, p_self with
                | UserPut,      UserPut
                | UserPut,      InternalPut
                | UserPut,      ForcePut
                | InternalPut,  UserPut
                | InternalPut,  InternalPut
                | InternalPut,  ForcePut ->
                    d_self#merge d_mergee undoer
                | ForcePut,     UserPut
                | ForcePut,     InternalPut -> (
                    self#remove name ForcePut;
                    self#put name f ForcePut undoer;
                    d_mergee#merge d_self undoer)
                | ForcePut,     ForcePut ->
                    assert false)
            | InternalPut, Directory _, Some (Symlink (s, _, _)) -> (
                (* There's a symlink of misdirection... follow it *)

                (* I can't quite fathom what to do in these cases so I'll
                 * forbid it for now *)
                 assert (String.substr_index s ~pattern:".." = None);
                 assert (String.substr_index s ~pattern:"/" = None);

                 self#put s f p undoer)
            | InternalPut, Directory _, Some (File _)
            | InternalPut, Directory _, Some (EphemeralFile _) -> assert false
            | UserPut, _, _ -> raise SecurityException
        method get name =
            Hashtbl.find hash name;
        method get_exn name =
            match self#get name with
            | Some x -> x
            | None -> raise Not_found
        method contains name = Hashtbl.mem hash name;
        method merge mergeme undoer =
            List.iter mergeme#ls (fun (x, y) -> self#put x y InternalPut undoer)
        method child_directory _ = new hash_directory;
        method receive _ = ();
    end;;

class writable_directory =
    object (self) inherit hash_directory as super
        method mkdir name mode put =
            super#mkdir name mode InternalPut
        method remove name put =
            super#remove name InternalPut
        method put name f p undoer = match p with
            | UserPut | InternalPut -> super#put name f InternalPut undoer
            | ForcePut -> super#put name f ForcePut undoer
        method child_directory _ = new writable_directory;
    end;;

let string_file read_fun =
    object (self) inherit file
        method read = read_fun () |> Bigstring.of_string
    end
;;

(* Proxy ramfile through to OS *)
class ramfile_builder ?initial:(initial="") stat_update =
    object (self) inherit file
        val filename = Filename.temp_file ~perm:0o600 "uvwpmacoh" "tmp"
        
        (* TODO: this is used a lot. Consider optimizing it (caching/memoization). *)
        method size = ((Unix.stat filename).st_size)

        method read = (
            let file_descriptor = Unix.openfile [Unix.O_RDONLY] filename 
            in
            let mmapped = Bigstring.map_file ~shared:false file_descriptor (Int64.to_int_exn self#size)
            in (
                Unix.close file_descriptor;
                mmapped
            )
        );

        method write (flaglist, offset64, l) = (
            let file_descriptor = Unix.openfile flaglist filename 
            in
            let _ = Unix.lseek file_descriptor ~mode:Unix.SEEK_SET offset64
            in
            let _ = Bigstring.write file_descriptor l
            in
            let () = Unix.close file_descriptor
            in
            stat_update ()
        );

        method truncate truncate_me_at =
            Unix.truncate filename ~len:truncate_me_at;
            stat_update ()

        method to_json = Some (
            "ramfile",
            (`String (Bigstring.to_string self#read)))

        initializer
            self#write ([Unix.O_WRONLY], Int64.zero, Bigstring.of_string initial)
    end

(* The big slash on the filesystem. Note that this is a singleton, and thus
 * this code only supports a single root.  *)
let root_filesystem = new hash_directory;;
let root_entry = Directory (
    root_filesystem,
    ImmutableStat (stat_factory S_DIR 0o755),
    InternalPut
);;

let url_file_factory url size =
    object (self) inherit file as super
        method read =
            Web.download_to_bigstring url

        method size = match size with
        | Some x -> x |> Int64.of_int
        | None -> super#size
    end;;


(* A bunch of functions to convert from the package format to their in-memory
representations. *)
let json_to_filesystem_functions = Hashtbl.create ~hashable:String.hashable ();;

let stat_from_json f kind =
let open Yojson.Basic.Util in
    match f with
    | `Null ->
        stat_factory
            kind
            (match kind with
            | S_DIR -> 0o755
            | S_LNK -> 0o777
            | S_REG -> 0o644
            | S_BLK
            | S_CHR
            | S_FIFO
            | S_SOCK -> assert false)
    | _ -> {
        (stat_factory
            kind
            (f |> member "perm" |> to_int))
        with
        st_size = Int64.of_int (match kind with
            | S_REG -> (
                f |>
                member "size" |>
                to_int_option |>
                Option.value ~default:0)
            | _ -> 0);
        st_ctime = f |> member "ctime" |> to_float;
        st_mtime = f |> member "mtime" |> to_float;
        st_atime = f |> member "mtime" |> to_float;
        st_uid = f |> member "uid" |> to_int;
        st_gid = f |> member "gid" |> to_int;
    }

let create_file_from_json json put =
let open Yojson.Basic.Util in
(
    (Hashtbl.find
            json_to_filesystem_functions
            (json |> member "deserializer" |> to_string)
            |> (function Some x -> x | None -> assert false))
        json
        put
);;

let json_stat f stat = match f with
    | Some (deserializer, x) ->
        Some (
            `Assoc ([
                ("stat",
                    `Assoc [
                        ("perm", `Int stat.st_perm);
                        ("ctime", `Float stat.st_ctime);
                        ("mtime", `Float stat.st_mtime);
                        ("uid", `Int stat.st_uid);
                        ("gid", `Int stat.st_gid);]);
                ("deserializer", `String deserializer);
                ("file", x)]))
    | None -> None;;


let serializer_filter put d =
    match put, d with
    | UserPut, _ -> d
    | _, Some (
        `Assoc ([
            ("stat", _);
            ("deserializer", `String "directory");
            ("file", `Assoc [])])) ->
                None
    | _, Some (
        `Assoc ([
            ("stat", _);
            ("deserializer", `String "directory");
            ("file", `Assoc _)])) ->
                d
    | _, _ -> None;;


type dependency =
    | VersionExplicit of string * string * string
    | VersionAny of string
    | VersionUnknown of string;;



let string_to_dependency package =
let chunker = Pcre.split ~rex:(Pcre.regexp "^[ ]*([^ ]+)( \\((.*) (.*)\\))?$")
in
match chunker package with
    | [""; x; _; comparator; version] ->
        VersionExplicit (x, comparator, version)
    | [""; x] ->
        VersionAny x
    | _ ->
        VersionUnknown package;;

exception Dependency_Fail;;

let rec first_viable package_list = match package_list with
    | [] -> raise Dependency_Fail
    | VersionExplicit (x, _, _) :: _ -> x
    | (VersionAny x) :: _ -> x
    | _ :: tail -> first_viable tail;;


let package_subdir package contents = Directory (
    object (self)
        method ls =
            [("name",
                File (
                    string_file (fun () -> package),
                    ImmutableStat (stat_factory S_REG 0o644),
                    InternalPut));
            ("contents",
                Directory (
                    contents,
                    ImmutableStat (stat_factory S_DIR 0o755),
                    InternalPut))];
        method mkdir =
            fun _ -> assert false;
        method remove =
            fun _ -> assert false;
        method get name =
            match List.hd (List.filter self#ls (fun (n2, _) -> name = n2)) with
            | Some (_, x) -> Some x
            | None -> None
        method get_exn name = match self#get name with
            | Some x -> x
            | None -> raise Not_found
        method put =
            fun _ _ -> assert false
        method contains =
            fun _ -> assert false
        method merge other_dir _ =
            if not (other_dir#ls = []) then
                assert false
        method child_directory _ = assert false
        method receive _ = ()
    end,
    ImmutableStat (stat_factory S_DIR 0o755),
    InternalPut
);;


(* This is very very naive, but I sort of like it that way for now. Mkdir
 * installs a package or one of a list of packages if they are | seperated.
 *
 * The package_directory *is* the package data. In future this should be
 * turned into more of a lens onto the actual data, which should be
 * represented with richer types. Orrrr maybe all package management should be
 * moved out of here and into programs which generate json. Hmm.
*)
let package_directory =
    object (self) inherit (hash_directory) as super
        method private extract_name package =
            match package with
                | VersionExplicit (x, _, _) -> x
                | VersionAny x -> x
                | VersionUnknown _ -> assert false

        method private already_installed package =
            super#contains (self#extract_name package)

        method private alternatives_from_name name = List.map
            (Pcre.split ~pat:" \\| " name)
            string_to_dependency

        method private perform_install package put = (
                let mergeme = (
                    match (("/extracts/" ^ package ^ ".json") |>
                            Web.download_to_string |>
                            Yojson.Basic.from_string |>
                            create_file_from_json) InternalPut
                    with
                    | Directory (d, _, _) -> d
                    | _ -> assert false)
                in (
                    let undo_list = ref [] in
                    let undoer x = undo_list := x::(!undo_list)
                    in
                    try
                        self#put
                            package
                            (package_subdir package mergeme)
                            ForcePut (* Make "put" *)
                            undoer;
                        root_filesystem#merge mergeme undoer
                    with e ->
                        log_string WARNING ("rolling back! " ^ package ^ "\n");
                        log_exception WARNING e;
                        flush_all ();
                        List.iter !undo_list (fun x -> x ())))

        method private symlink_housekeeping real sym =
            match real = sym, self#contains sym with
            | false, false ->
                super#put sym (
                    Symlink (
                        real,
                        ImmutableStat (stat_factory S_LNK 0o777),
                        InternalPut))
                    ForcePut
                    ignore;
            | false, true
            | true, false
            | true, true -> ()

        method mkdir name mode put =
            (* This is the "install package" routine *)
            match self#contains name with
            | false ->
                let alternatives = self#alternatives_from_name name
                in
                let installed_alternative =
                    List.find
                        ~f:self#contains
                        (List.map ~f:self#extract_name alternatives)
                in
                    (match installed_alternative with
                    | Some x -> self#symlink_housekeeping x name
                    | None -> (
                        let installme = first_viable alternatives
                        in (
                            self#symlink_housekeeping installme name;
                            self#perform_install installme put)))
            | true -> ()

        method remove name p =
            match p with
            | ForcePut -> super#remove name p
            | UserPut
            | InternalPut -> assert false

    end;;



let system_serializer () =
    let rec system_serializer_helper file =
    match file with
    | Directory (d, stat, p) ->
        json_stat (
            Some (
                "directory",
                `Assoc (
                    List.fold_left
                        d#ls
                        ~init:[]
                        ~f:(
                            fun acc (key, data) ->
                                match system_serializer_helper data with
                                | Some x -> (key, x) :: acc
                                | None -> acc))))
        (unbox_stat stat) |>
        serializer_filter p
    | Symlink (s, stat, p) ->
        json_stat
            (Some ("symlink", `String s)) (unbox_stat stat) |>
            serializer_filter p
    | File (f, stat, p) ->
        json_stat
            f#to_json
            (unbox_stat stat) |>
            serializer_filter p
    | EphemeralFile _ (* Mount _ | *) -> assert false
in
    match system_serializer_helper root_entry with
    | Some x -> Yojson.Basic.pretty_to_string x
    | None -> raise (Failure "Could not serialize")
;;


let daemon_directory_factory daemonname = (
    let argv_dir =
    object (self) inherit writable_directory as super
        method mkdir _ = assert false;
    end
    in
    let extract_string_from_file f = match f with
    | File (f, _, _) -> Bigstring.to_string f#read
    | _ -> assert false
    in
    let sort_by_filename =
        List.sort ~cmp:(fun (a, _) (b, _) -> String.compare a b)
    in
    let argv () =
        argv_dir#ls |>
        sort_by_filename |>
        List.map ~f:snd |>
        List.map ~f:extract_string_from_file
    in
    let enabled = ref false
    in
    let boink () =
        if !enabled then
            Pipes.init_writer (Pipes.DaemonEnable (daemonname, [], argv ()))
        else ()
    in
    let ret =
    object (self) inherit hash_directory as super
        method mkdir _ = assert false;
        method put name f p = match p with
            | UserPut -> raise SecurityException
            | _ -> super#put name f p;
        method receive message =
            match message with
            | Pipes.DaemonStarted (_, pid) ->
                self#put "pid"
                    (Symlink
                        ("/proc/" ^ Pid.to_string pid,
                        ImmutableStat (stat_factory S_LNK 0o777),
                        InternalPut))
                    InternalPut
                    ignore
            | Pipes.DaemonDied _ ->
                self#remove "pid" InternalPut;
                boink ()
            | _ -> assert false
    end
    in
    let enabler =
        object (self) inherit file
            method read = !enabled |> Bool.to_string |> Bigstring.of_string
            method write (_, _, contents) =
                enabled := Bool.of_string (Bigstring.to_string contents);
                boink ()
            method truncate _ = ()
        end
    in (
        ret#put "argv" (
            Directory (
                argv_dir,
                ImmutableStat (stat_factory S_DIR 0o755),
                InternalPut))
            ForcePut
            ignore;

        ret#put "enabled" (
            File (
                enabler,
                ImmutableStat (stat_factory S_REG 0o644),
                InternalPut))
            ForcePut
            ignore;
        ret));;

let daemon_metadirectory =
    object (self) inherit writable_directory as super
        method child_directory name = daemon_directory_factory name
    end;;


let add_deserializer = Hashtbl.add_exn json_to_filesystem_functions
in
let open Yojson.Basic.Util in
(
    add_deserializer
        "symlink"
        (fun json put ->
            Symlink (
                (json |> member "file" |> to_string),
                ImmutableStat (stat_from_json (json |> member "stat") S_LNK),
                put));

    add_deserializer
        "url"
        (fun json put -> File (
            url_file_factory
                (json |>
                member "file" |>
                to_string)
                (json |>
                member "stat" |>
                member "size" |>
                to_int_option),
            ImmutableStat (stat_from_json (json |> member "stat") S_REG),
            put));

    add_deserializer
        "ramfile"
        (fun json put -> File (
            new ramfile_builder
                ~initial:(json |> member "file" |> to_string)
                (fun () -> ()),
            ImmutableStat (stat_from_json (json |> member "stat") S_REG),
            put));

    add_deserializer
        "overwrite"
        (fun json put -> EphemeralFile (
            fun f ->
                f#write (
                        [], (* yuck *)
                        Int64.of_int 0, (* yuck *)
                        json |>
                        member "file" |>
                        to_string |>
                        Bigstring.of_string)));

    add_deserializer
        "directory"
        (fun json put ->
            let ret = new hash_directory
            in (
                List.iter
                    (json |> member "file" |> to_assoc)
                    (fun (f, v) ->
                        ret#put
                            f
                            (create_file_from_json v put)
                            InternalPut
                            ignore);
                Directory (
                    ret,
                    ImmutableStat (
                        stat_from_json
                        (json |> member "stat")
                        S_DIR),
                    put)));

        (*
    add_deserializer
        "mount"
        (fun json _ ->
            Mount (
                    json |> member "type" |> to_string,
                    json |> member "device" |> to_string,
                    json |> member "options" |> to_string_option
                )
        );*)

    add_deserializer
        "ramfs"
        (fun json _ ->
            Directory (
                new writable_directory,
                ImmutableStat (stat_from_json (json |> member "stat") S_DIR),
                ForcePut));

    add_deserializer
        "control"
        (fun json put -> (
                let control_directory = new hash_directory
                in
                (
                control_directory#put "system" (
                    File (
                        string_file system_serializer,
                        ImmutableStat (stat_factory S_REG 0o666),
                        InternalPut)) ForcePut ignore;

                control_directory#put "packages" (
                    Directory (
                        package_directory,
                        ImmutableStat (stat_factory S_DIR 0o755),
                        InternalPut)) ForcePut ignore;

                control_directory#put "log" (
                    File (
                        string_file (fun () -> Buffer.contents logs),
                        ImmutableStat (stat_factory S_REG 0o644),
                        InternalPut)) ForcePut ignore;

                control_directory#put "log_to_console" (
                    File (
                        object (self) inherit file
                            method read = (
                                !log_to_console |>
                                (function
                                | DEBUG -> "DEBUG"
                                | WARNING -> "WARNING"
                                | INFO -> "INFO"
                                | NONE -> "NONE") |>
                                Bigstring.of_string);

                            method write (_, _, buf) =
                                buf |>
                                Bigstring.to_string |>
                                String.uppercase |>
                                (function
                                | "DEBUG" -> DEBUG
                                | "WARNING" -> WARNING
                                | "INFO" -> INFO
                                | "NONE" -> NONE
                                | _ -> assert false) |>
                                (fun x -> log_to_console := x);

                            method truncate _ = ()
                        end,
                        ImmutableStat (stat_factory S_REG 0o644),
                        InternalPut)) ForcePut ignore;

                control_directory#put
                    "daemons"
                    (Directory (
                        daemon_metadirectory,
                        ImmutableStat (stat_factory S_DIR 0o755),
                        InternalPut)) ForcePut ignore;

                (* Now add the "file" in the json... *)
                List.iter
                    (json |> member "file" |> to_assoc)
                    (fun (f, v) ->
                        control_directory#put
                            f
                            (create_file_from_json v put)
                            InternalPut
                            ignore);

                (* Return this! *)
                Directory (
                    control_directory,
                    ImmutableStat (
                        stat_from_json
                            (json |> member "stat")
                            S_DIR),
                            ForcePut)))));;


(*
* Aggregate directories allow you do have:
*
* $ echo hello > foo.d/this
* $ echo world > foo.d/that
* $ cat foo
* hello
* world
*
* This should be useful for sitations like:
* - configs using incldues where it's not natively supported
* - /etc/passwd
*
* This *ignores* symlinks and directories. Probably need to rethink this and
* recurse.
*)
let aggregate_directory () = (
    let dir = new hash_directory
    in
    let file = (
        object (self) inherit file
            method read = (
                let bigarrays = List.map
                    dir#ls
                    (
                        fun (_, foo) -> match foo with
                        | File (x, _, _) -> x#read
                        | _ ->
                            Bigarray.Array1.create
                                Bigarray.char
                                Bigarray.c_layout
                                0)
            in
            let new_array =
                Bigarray.Array1.create
                    Bigarray.char
                    Bigarray.c_layout
                    (
                        List.fold_left
                            bigarrays
                            ~init:0
                            ~f:(fun y x -> (Bigarray.Array1.dim x) + y))
            in
            (* Okay, this is a bit confusing. At least it was confusing to
             * write!
             *
             * We've /already/ created the array we want to return. We now want
             * to take subarrays of that and blit over them with the actual
             * contents of the directory *)
            ignore (List.fold_left
                bigarrays
                ~f:(fun tmp_array next_array -> (
                    let next_array_length = Bigarray.Array1.dim next_array
                    in
                    let tmp_array_length = Bigarray.Array1.dim tmp_array
                    in (
                        Bigarray.Array1.blit next_array (
                            Bigarray.Array1.sub
                                tmp_array
                                0
                                (Bigarray.Array1.dim next_array));
                        Bigarray.Array1.sub
                            tmp_array
                            next_array_length
                            (tmp_array_length - next_array_length))))
                ~init:new_array);
            new_array)
        end) in (
            (dir, ImmutableStat (stat_factory S_DIR 0o755)),
            (file, ImmutableStat (stat_factory S_REG 0o644))));;



let rec fuse_reader_loop () = (
    let message = Pipes.fuse_reader ()
    in
    (match message with
        | Pipes.InitReady -> purge_deferred ()
        | Pipes.DaemonStarted (name, pid) -> (
            match daemon_metadirectory#get_exn name with
            | Directory (d, _, _) -> d#receive message
            | _ -> assert false)
        | Pipes.DaemonDied name -> (
            match daemon_metadirectory#get_exn name with
            | Directory (d, _, _) -> d#receive message
            | _ -> assert false)
        | Pipes.ResolvConf resolvconf -> (
            (match (root_filesystem#get_exn "etc") with
            | Directory (etc, _, _) -> etc
            | _ -> assert false) #put "resolv.conf" (
                File (
                    string_file (fun () -> resolvconf),
                    ImmutableStat (stat_factory S_REG 0o644),
                    InternalPut)) InternalPut ignore));
    fuse_reader_loop ());;


let build_filesystem definition = (
    let etc_directory = new hash_directory
    in (
        root_filesystem#put "etc" (
            Directory (
                etc_directory,
                ImmutableStat (stat_factory S_DIR 0o755),
                InternalPut)) ForcePut ignore;

        match aggregate_directory () with
            ((d, ds), (f, fs)) -> (
                etc_directory#put
                    "passwd"
                    (File (f, fs, InternalPut))
                    ForcePut
                    ignore;
                etc_directory#put
                    "passwd.d"
                    (Directory (d, ds, InternalPut))
                    ForcePut
                    ignore);

        match aggregate_directory () with
            ((d, ds), (f, fs)) -> (
                let fs_ = ImmutableStat { (unbox_stat fs) with st_perm = 0o400 }
                in
                let ds_ = ImmutableStat { (unbox_stat ds) with st_perm = 0o700 }
                in (
                    etc_directory#put
                        "shadow"
                        (File (f, fs_, InternalPut))
                        ForcePut
                        ignore;
                    etc_directory#put
                        "shadow.d"
                        (Directory (d, ds_, InternalPut))
                        ForcePut
                        ignore));

        match aggregate_directory () with
            ((d, ds), (f, fs)) -> (
                etc_directory#put
                    "group"
                    (File (f, fs, InternalPut))
                    ForcePut
                    ignore;
                etc_directory#put
                    "group.d"
                    (Directory (d, ds, InternalPut))
                    ForcePut
                    ignore);

        root_filesystem#merge (
            match
                (definition |>
                Web.download_to_string |>
                Yojson.Basic.from_string |>
                create_file_from_json) UserPut
            with
            | Directory (d, _, _) -> d
            | _ -> assert false) ignore));;
