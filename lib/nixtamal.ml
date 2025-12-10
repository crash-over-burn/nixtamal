(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Error = Error
module Name = Name
module Manifest = Manifest
module Input = Input
module Input_foreman = Input_foreman
module Working_directory = Working_directory

type error = Error.error

let meld_input_with_lock (input : Input.t) (lock : Lockfile.Input'.t) : Input.t = {input with
	kind = (
		match input.kind, lock.kind with
		| `File file, `File _ ->
			`File file
		| `Archive archive, `Archive _ ->
			`Archive archive
		| `Git git, `Git({latest_revision; _}: Lockfile.Git.t) ->
			`Git {git with latest_revision}
		| `Darcs darcs, `Darcs({reference; latest_weak_hash; _}: Lockfile.Darcs.t) ->
			`Darcs {darcs with
				latest_weak_hash;
				reference = (
					match darcs.reference, reference with
					| `Context (`Assumed None), `Context (`Assumed Some _) -> reference
					| _ -> darcs.reference
				);
			}
		| `Pijul pijul, `Pijul({latest_state; _}: Lockfile.Pijul.t) ->
			`Pijul {pijul with latest_state}
		| _, _ -> failwith "Input kind mismatch."
	);
	hash = {input.hash with value = lock.hash.value};
	latest = {input.latest with value = lock.latest_value}
}

let read_manifest_and_lockfile () : (Name.Name.t list, error) result =
	let (let*) = Result.bind in
	let* manifest =
		Error.tag_manifest @@ begin
			match Manifest.read () with
			| Ok(kdl : Kdl.t) ->
				Manifest.document_to_t kdl
				|> Result.map_error (fun err -> `Parsing err)
			| Error(e : Kdl.error) ->
				let v_errs : Util.KDL.Valid.err list = [`ParseError e] in
				Error (`Parsing v_errs)
		end
	in
	let* lockfile_opt =
		Error.tag_lockfile @@ begin
			Lockfile.read ()
			|> Result.map_error (fun e -> `Parsing e)
		end
	in
	match lockfile_opt with
	| Some lock when not (String.equal manifest.version lock.version) ->
		Error (`Version_mismatch (manifest.version, lock.version))
	| _ ->
		let to_input d =
			let input = Manifest.Input'.of_manifest d in
			let found_input =
				Option.bind lockfile_opt (fun lock -> Name.NameMap.find_opt input.name lock.inputs)
			in
			match found_input with
			| None -> input
			| Some locked -> meld_input_with_lock input locked
		in
		Error.tag_input_foreman @@ begin
			let rec set_input_htbl ret mnfsts =
				match ret, mnfsts with
				| Error err, _ -> Error err
				| Ok names, [] -> Ok names
				| Ok names, d :: ds ->
					let input : Input.t = to_input d in
					let res =
						Input_foreman.add input.name input
						|> Result.map (fun () -> input.name :: names)
					in
					set_input_htbl res ds
			in
			let* names = set_input_htbl (Ok []) manifest.inputs in
			Logs.debug (fun m -> m "Names from reckoning manifest × lockfile: %a" Fmt.(brackets (list ~sep: semi Name.Name.pp)) names);
			Ok names
		end

let set_up ~env ?nixpkgs: nixpkgs_opt () : (unit, error) result =
	let (let*) = Result.bind in
	Eio.Switch.run @@ fun sw ->
	let proc_mgr = Eio.Stdenv.process_mgr env in
	Logs.app (fun m -> m "%t@." Banner.pp);
	Working_directory.set_up_root ();
	if Manifest.exists () then
		begin
			Logs.err (fun m ->
				m
					"Found existing “%s” file, so project is already set up. Quitting."
					Manifest.filename
			);
			Error (`Manifest `File_already_exists)
		end
	else
		(* TODO: returns a bool for success, so what to do? *)
		let* () =
			match nixpkgs_opt with
			| None -> Ok ()
			| Some npkgs ->
				let* () =
					Error.tag_input_foreman @@ begin
						let* () = Input_foreman.add Input.Nixpkgs.name npkgs in
						Input_foreman.lock_one ~env ~sw ~proc_mgr ~force: false ~name: Input.Nixpkgs.name
					end
				in
				let _lockfile = Lockfile.make () in
				Ok ()
		in
		let* () =
			Error.tag_manifest @@ begin
				let () = Manifest.make () in
				Manifest.write ()
			end
		in
		let* () =
			Error.tag_lockfile @@ begin
				Lockfile.write ~create: (`Exclusive 0o644) ()
			end
		in
		Lock_loader.write ();
		Ok ()

let check_soundness ~env: _ () : (unit, error) result =
	let (let*) = Result.bind in
	let* _all_names = read_manifest_and_lockfile () in
	Logs.app (fun m -> m "All sound.");
	Ok ()

let tweak ~env () : (unit, error) result =
	let working_dir = Working_directory.get () in
	let manifest_file : string =
		let path = Eio.Path.(working_dir / Manifest.filename) in
		Eio.Path.native_exn path
	in
	let () = Editor.run_on manifest_file in
	check_soundness ~env ()

let show ~env: _ () : (unit, error) result =
	let (let*) = Result.bind in
	let* _all_names = read_manifest_and_lockfile () in
	Logs.app (fun m -> m "%t" Input_foreman.pp_for_earthlings);
	Ok ()

let lock ~env ~domain_count ?(force = false) ?names () : (unit, error) result =
	Eio.Switch.run @@ fun sw ->
	let (let*) = Result.bind in
	let proc_mgr = Eio.Stdenv.process_mgr env in
	let* all_names = read_manifest_and_lockfile () in
	let* () =
		Error.tag_input_foreman @@ begin
			let names =
				match names with
				| None | Some [] -> all_names
				| Some ns -> ns
			in
			Input_foreman.lock ~env ~sw ~proc_mgr ~domain_count ~force ~names ()
		end
	in
	let* () = Error.tag_lockfile @@ Lockfile.write () in
	Lock_loader.write ();
	Ok ()

let refresh ~env ~domain_count ?names () : (unit, error) result =
	Eio.Switch.run @@ fun sw ->
	let (let*) = Result.bind in
	let proc_mgr = Eio.Stdenv.process_mgr env in
	let* all_names = read_manifest_and_lockfile () in
	let* () =
		Error.tag_input_foreman @@ begin
			let names =
				match names with
				| None | Some [] -> all_names
				| Some ns -> ns
			in
			Input_foreman.refresh ~env ~sw ~proc_mgr ~domain_count ~names ()
		end
	in
	let* () = Error.tag_lockfile @@ Lockfile.write () in
	Lock_loader.write ();
	Ok ()
