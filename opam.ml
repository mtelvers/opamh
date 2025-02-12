open Bos

let opam = Cmd.v "opam"
let switch = ref None
let prefix = ref None

type package = { name : string; version : string }

let pp fmt p = Format.fprintf fmt "%s.%s" p.name p.version

let rec get_switch () =
  match !switch with
  | None ->
      let cur_switch =
        Util.lines_of_process Cmd.(opam % "switch" % "show") |> List.hd
      in
      switch := Some cur_switch;
      get_switch ()
  | Some s -> s

let prefix () =
  match !prefix with
  | Some p -> p
  | None ->
      let p =
        Util.lines_of_process
          Cmd.(opam % "var" % "--switch" % get_switch () % "prefix")
        |> List.hd
      in
      prefix := Some p;
      p

let deps_of_opam_result line =
  match Astring.String.fields ~empty:false line with
  | [ name; version ] -> [ { name; version } ]
  | _ -> []

let all_opam_packages () =
  Util.lines_of_process
    Cmd.(
      opam % "list" % "--switch" % get_switch () % "--columns=name,version"
      % "--color=never" % "--short")
  |> List.map deps_of_opam_result
  |> List.flatten

let pkg_contents { name; version } =
  let prefix = Fpath.v (prefix ()) in
  let changes_file =
    Format.asprintf "%a/.opam-switch/install/%s.changes" Fpath.pp prefix name
  in
  let file = OpamFilename.raw changes_file in
  let filename =
    OpamFile.make @@ OpamFilename.raw @@ Filename.basename changes_file
  in
  let changed =
    OpamFilename.with_contents
      (fun str ->
        OpamFile.Changes.read_from_string ~filename
        @@
        (* Field [opam-version] is invalid in [*.changes] files, displaying a warning. *)
        if OpamStd.String.starts_with ~prefix:"opam-version" str then
          match OpamStd.String.cut_at str '\n' with
          | Some (_, str) -> str
          | None -> assert false
        else str)
      file
  in
  let added =
    OpamStd.String.Map.fold
      (fun file x acc ->
        match x with
        | OpamDirTrack.Added _ -> (
            try
              if not @@ Sys.is_directory Fpath.(to_string (normalize (prefix // v file)))
              then (
                Format.eprintf "File: %s\n%!" Fpath.(to_string (normalize (v file)));
                Fpath.(to_string (normalize (v file))) :: acc)
              else acc
            with _ ->
              acc
              (* dose (and maybe others) sometimes creates a symlink to something that doesn't exist *)
            )
        | _ -> acc)
      changed []
  in
  let opam_file = Format.asprintf ".opam-switch/packages/%s.%s/opam" name version in
  let changes_file = Format.asprintf ".opam-switch/install/%s.changes" name in

  let added = changes_file :: opam_file :: added in

  List.map Fpath.v added


let installed_packages () =
  let prefix = Fpath.v (prefix ()) in
  let state_file = Format.asprintf "%a/.opam-switch/switch-state" Fpath.pp prefix in
  let file = OpamFilename.raw state_file in
  let filename =
    OpamFile.make @@ OpamFilename.raw @@ Filename.basename state_file
  in
  let state = OpamFilename.with_contents
    (fun str ->
      OpamFile.SwitchSelections.read_from_string ~filename str) file in
  let installed = OpamPackage.Set.to_list state.sel_installed in
  installed

let dump_state output quiet =
  let prefix = Fpath.v (prefix ()) in
  let state_file = Format.asprintf "%a/.opam-switch/switch-state" Fpath.pp prefix in
  let file = OpamFilename.raw state_file in
  let filename =
    OpamFile.make @@ OpamFilename.raw @@ Filename.basename state_file
  in
  let state = OpamFilename.with_contents
    (fun str ->
      OpamFile.SwitchSelections.read_from_string ~filename str) file in
  (* let names = OpamPackage.Set.fold (fun pkg acc -> OpamPackage.to_string pkg :: acc) state.sel_installed [] in *)
  let compiler = OpamPackage.Set.fold (fun pkg acc -> OpamPackage.to_string pkg :: acc) state.sel_compiler [] in
  if not quiet then
    List.iter (fun x ->
      Format.eprintf "SEL_COMPILER: %s\n%!" x) compiler;
    
  let packages_dir = Fpath.v (Format.asprintf "%a/.opam-switch/packages" Fpath.pp prefix) in

  let installed = Bos.OS.Dir.contents packages_dir in
  match installed with
  | Error _ -> Format.eprintf "Error reading contents of packages directory: %a" Fpath.pp packages_dir;
    failwith "Error"
  | Ok content ->
    let packages = List.filter_map (fun x -> OpamPackage.of_string_opt (Fpath.basename x)) content in
    let compiler_packages = List.map OpamPackage.Name.of_string [
      "ocaml"; "ocaml-base-compiler"; "ocaml-options-vanilla"; "base-bigarray"; "base-domains"; "base-nnp"; "base-threads"; "base-unix"; "host-arch-x86"; "host-system-other"
    ] in
    let sel_compiler = List.filter (fun x ->
      List.mem (OpamPackage.name x) compiler_packages
    ) packages in
    if not quiet then (
      Format.eprintf "Packages:\n%!";
      Format.eprintf "[%a]" Fmt.(list ~sep:comma (of_to_string OpamPackage.to_string)) packages
    );
  let new_state =
    let s = OpamPackage.Set.of_list packages in
    { OpamTypes.sel_installed = s;
      sel_roots = s;
      sel_pinned = OpamPackage.Set.empty;
      sel_compiler = OpamPackage.Set.of_list sel_compiler;
    }
  in

  let _ =
    OpamFilename.write (OpamFilename.raw output) (OpamFile.SwitchSelections.write_to_string new_state) in
    
  OpamPackage.Set.iter (fun x -> Format.fprintf Format.std_formatter "%s\n%!" (OpamPackage.to_string x)) state.sel_installed;
  ()

(* let opam_file { name; version } = *)
(*   let prefix = Fpath.v (prefix ()) in *)
(*   let opam_file = *)
(*     Format.asprintf "%a/.opam-switch/packages/%s.%s/opam" Fpath.pp prefix name *)
(*       version *)
(*   in *)
(*   let ic = open_in opam_file in *)
(*   try *)
(*     let lines = Util.lines_of_channel ic in *)
(*     close_in ic; *)
(*     Some lines *)
(*   with _ -> *)
(*     close_in ic; *)
(*     None *)

type installed_files = {
  libs : Fpath.set;
  odoc_pages : Fpath.set;
  other_docs : Fpath.set;
}

type package_of_fpath = package Fpath.map

(* Here we use an associative list *)
type fpaths_of_package = (package * installed_files) list

let pkg_to_dir_map () =
  let pkgs = all_opam_packages () in
  let prefix = prefix () in
  let pkg_content =
    List.map
      (fun p ->
        let contents = pkg_contents p in
        let libs =
          List.fold_left
            (fun set fpath ->
              match Fpath.segs fpath with
              | "lib" :: "stublibs" :: _ -> set
              | "lib" :: _ :: _ :: _ when Fpath.has_ext ".cmi" fpath ->
                  Fpath.Set.add
                    Fpath.(v prefix // fpath |> split_base |> fst)
                    set
              | _ -> set)
            Fpath.Set.empty contents
        in
        let odoc_pages, other_docs =
          List.fold_left
            (fun (odoc_pages, others) fpath ->
              match Fpath.segs fpath with
              | "doc" :: _pkg :: "odoc-pages" :: _ ->
                  Logs.debug (fun m -> m "Found odoc page: %a" Fpath.pp fpath);

                  (Fpath.Set.add Fpath.(v prefix // fpath) odoc_pages, others)
              | "doc" :: _ ->
                  Logs.debug (fun m -> m "Found other doc: %a" Fpath.pp fpath);
                  (odoc_pages, Fpath.Set.add Fpath.(v prefix // fpath) others)
              | _ -> (odoc_pages, others))
            Fpath.Set.(empty, empty)
            contents
        in
        Logs.debug (fun m ->
            m "Found %d odoc pages, %d other docs"
              (Fpath.Set.cardinal odoc_pages)
              (Fpath.Set.cardinal other_docs));
        (p, { libs; odoc_pages; other_docs }))
      pkgs
  in
  let map =
    List.fold_left
      (fun map (p, { libs; _ }) ->
        Fpath.Set.fold
          (fun dir map ->
            Fpath.Map.update dir
              (function
                | None -> Some p
                | Some x ->
                    Logs.debug (fun m ->
                        m "Multiple packages (%a,%a) found for dir %a" pp x pp p
                          Fpath.pp dir);
                    Some p)
              map)
          libs map)
      Fpath.Map.empty pkg_content
  in
  (pkg_content, map)

