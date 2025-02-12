
let save name pkgs =
  let installed = Opam.installed_packages () in
  let contents = List.fold_left
    (fun acc pkg ->
      let opam_pkg =
        try List.find (fun p -> OpamPackage.name p |> OpamPackage.Name.to_string = pkg) installed
      with Not_found ->
        Format.eprintf "Can't find package %s" pkg;
      exit 1 in
    
      let version = OpamPackage.version opam_pkg |> OpamPackage.Version.to_string in
      let package = { Opam.name=pkg; version } in
      let contents = Opam.pkg_contents package in
      contents @ acc
      ) [] pkgs in
  if List.length contents > 0 then 
    let prefix = Opam.prefix () in
    let cmd = Bos.Cmd.(v "tar" % "cf" % name) in
    let cmd = List.fold_left (fun cmd f ->
      Bos.Cmd.(cmd % (prefix ^ "/" ^ Fpath.to_string f))) cmd contents in
    ignore (Util.lines_of_process cmd)


open Cmdliner

let save_cmd =
  let package_name =
    let doc = "Name of package" in
    Arg.(value & pos_all string [] & info [] ~docv:"NAME"
           ~doc)
  in
  let file =
    let doc = "Output filename" in
    Arg.(required & opt (some string) None & info ["o"; "output"] ~docv:"FILENAME"
      ~doc)
  in
  let doc = "Save a package" in
  let info = Cmd.info "save" ~doc in
  Cmd.v info Term.(const save $ file $ package_name)

let make_state_cmd =
  let file =
    let doc = "Output filename" in
    Arg.(required & opt (some string) None & info ["o"; "output"] ~docv:"FILENAME"
      ~doc)
  in
  let quiet =
    let doc = "No output" in
    Arg.(value & flag & info ["quiet"] ~docv:"QUIET" ~doc)
  in
  let doc = "Reconstruct the opam state from the installed packages" in
  let info = Cmd.info "make-state" ~doc in
  Cmd.v info Term.(const Opam.dump_state $ file $ quiet)

let main_cmd =
  let doc = "Opam hijinx" in
  let info = Cmd.info "opamh" ~doc in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())) in
  Cmd.group info ~default [make_state_cmd; save_cmd]

  let () = exit (Cmd.eval main_cmd)
