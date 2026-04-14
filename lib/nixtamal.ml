(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Error = Error
module Name = Name
module Manifest = Manifest
module Lockfile = Lockfile
module Input = Input
module Input_foreman = Input_foreman
module Working_directory = Working_directory
module KDL = KDL
module Schema = Schema

type error = Error.error

let meld_input_with_lock (input : Input.t) (lock : Lockfile.Input'.t) : Input.t = {input with
	kind = (
		match input.kind, lock.kind with
		| `File file, `File _ ->
			`File file
		| `Archive archive, `Archive _ ->
			`Archive archive
		| `Git git, `Git({datetime; latest_revision; _}: Lockfile.Git.t) ->
			`Git {git with datetime; latest_revision}
		| `Darcs darcs, `Darcs({datetime; reference; latest_weak_hash; _}: Lockfile.Darcs.t) ->
			`Darcs {darcs with
				datetime;
				latest_weak_hash;
				reference = (
					match darcs.reference, reference with
					| `Context (`Assumed None), `Context (`Assumed Some _) -> reference
					| _ -> darcs.reference
				);
			}
		| `Pijul pijul, `Pijul({datetime; latest_state; _}: Lockfile.Pijul.t) ->
			`Pijul {pijul with datetime; latest_state}
		| `Nilla nilla, `Nilla({datetime; latest_revision; _}: Lockfile.Nilla.t) ->
			`Nilla {nilla with datetime; latest_revision}
		| `Fossil fossil, `Fossil({datetime; latest_checkin; _}: Lockfile.Fossil.t) ->
			`Fossil {fossil with date = datetime; latest_checkin}
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
			| Ok(kdl : KDL.t) ->
				Manifest.document_to_t kdl
				|> Result.map_error (fun err -> `Parsing err)
			| Error(e : KDL.error) ->
				let v_errs : KDL.Valid.err list = [`ParseError e] in
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
		let lockfile_opt = match lockfile_opt with
			| Some lock -> Some lock
			| None ->
				Logs.info (fun m -> m "Lockfile missing, creating new empty lockfile");
				match Lockfile.make ~version: manifest.version () with
				| Ok lock -> Some lock
				| Error e ->
					Logs.warn (fun m -> m "Failed to create lockfile: %a" Error.pp_lockfile_error e);
					None
		in
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
			Logs.warn (fun m ->
				m
					"Found existing “%s” file, so project is already set up. Skipping."
					Manifest.filename
			);
			Ok ()
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
	Input_foreman.clean_unlisted_from_silo ();
	Ok ()

let list_stale ~env ~domain_count : (unit, error) result =
	Eio.Switch.run @@ fun sw ->
	let (let*) = Result.bind in
	let proc_mgr = Eio.Stdenv.process_mgr env in
	let* all_names = read_manifest_and_lockfile () in
	Error.tag_input_foreman @@ begin
		Input_foreman.list_stale ~env ~sw ~proc_mgr ~domain_count ~names: all_names
	end

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
	Input_foreman.clean_unlisted_from_silo ();
	Ok ()

let backup_path (filename : string) : string = filename ^ ".bak"

let upgrade ?from ?(to_ = Schema.Version.current) ?(dry_run = false) () : (unit, error) result =
	let (let*) = Result.bind in
	let working_dir = Working_directory.get () in
	let lockfile_path = Eio.Path.(working_dir / Lockfile.filename) in
	let manifest_path = Eio.Path.(working_dir / Manifest.filename) in
	let manifest_content =
		Eio.Path.with_open_in manifest_path @@ fun flow ->
		let buf = Eio.Buf_read.of_flow flow ~max_size: max_int in
		Eio.Buf_read.take_all buf
	in
	Logs.info (fun m -> m "Current schema version: %a" Schema.Version.pp Schema.Version.current);
	let* manifest_version =
		let open KDL.L in
		let open KDL.Valid in
		match Eio.Path.with_open_in manifest_path KDL.of_flow with
		| Error _ -> Error (`Manifest `Not_set_up)
		| Ok kdl_doc ->
			match ll @@ kdl_doc.@(node "version" // arg 0 // string_value) with
			| Error _ -> Error (`Upgrade "Manifest version not found")
			| Ok version_str ->
				match Schema.Version.of_string version_str with
				| Some v -> Ok v
				| None -> Error (`Upgrade (Fmt.str "Unknown manifest schema version: %s" version_str))
	in
	Logs.info (fun m -> m "Manifest version: %a" Schema.Version.pp manifest_version);
	let* lockfile_version =
		match Lockfile.read () with
		| Ok (Some lock) ->
			begin match Schema.Version.of_string lock.version with
			| Some v -> Ok v
			| None -> Error (`Upgrade (Fmt.str "Unknown lockfile schema version: %s" lock.version))
			end
		| Ok None -> Error (`Upgrade "Lockfile missing, cannot determine version")
		| Error err -> Error (`Lockfile (`Parsing err))
	in
	Logs.info (fun m -> m "Lockfile version: %a" Schema.Version.pp lockfile_version);
	let* () =
		match from with
		| None -> Ok ()
		| Some from' when from' <> manifest_version ->
			Error (`Version_mismatch (Schema.Version.to_string manifest_version, Schema.Version.to_string from'))
		| Some from' when from' <> lockfile_version ->
			Error (`Version_mismatch (Schema.Version.to_string lockfile_version, Schema.Version.to_string from'))
		| Some _ -> Ok ()
	in
	let needs_manifest_upgrade = manifest_version < to_
	and needs_lock_upgrade = lockfile_version < to_
	in
	if not needs_lock_upgrade && not needs_manifest_upgrade then begin
		Logs.app (fun m -> m "Already at %a" Schema.Version.pp to_);
		Ok ()
	end else if dry_run then begin
		if needs_manifest_upgrade then
			Logs.app (fun m ->
				m "Would upgrade %s: %a → %a"
				Manifest.filename Schema.Version.pp manifest_version Schema.Version.pp to_
			);
		if needs_lock_upgrade then
			Logs.app (fun m ->
				m "Would upgrade %s: %a → %a"
				Lockfile.filename Schema.Version.pp lockfile_version Schema.Version.pp to_
			);
		Logs.app (fun m -> m "No changes applied.");
		Ok ()
	end
	else
		let manifest_backup_path = Eio.Path.(working_dir / backup_path Manifest.filename)
		and lock_backup_path = Eio.Path.(working_dir / backup_path Lockfile.filename)
		and manifest_backup_created = ref false
		and lock_backup_created = ref false
		in
		let cleanup_backups () =
			Logs.info (fun m -> m "Cleaning up backups…");
			if !manifest_backup_created then
				try Eio.Path.unlink manifest_backup_path
				with _ -> Logs.err (fun m -> m "Failed to cleanup manifest backup");
			if !lock_backup_created then
				try Eio.Path.unlink lock_backup_path
				with _ -> Logs.err (fun m -> m "Failed to cleanup lock backup")
		in
		let rollback () =
			Logs.info (fun m -> m "Rolling back backups…");
			if !manifest_backup_created then
				try Eio.Path.rename manifest_backup_path manifest_path
				with _ -> Logs.err (fun m -> m "Failed to rollback manifest");
			if !lock_backup_created then
				try Eio.Path.rename lock_backup_path lockfile_path
				with _ -> Logs.err (fun m -> m "Failed to rollback lock");
			cleanup_backups ()
		in
		try
			let* () =
				if needs_lock_upgrade then
					match Lockfile.read () with
					| Error err -> Error (`Lockfile (`Parsing err))
					| Ok lockfile_opt ->
						match lockfile_opt with
						| None -> Error (`Upgrade "Lockfile not set up for upgrade")
						| Some lockfile ->
							Logs.app (fun m -> m "Upgrading %s: %a → %a" Lockfile.filename Schema.Version.pp lockfile_version Schema.Version.pp to_);
							let upgraded = {lockfile with version = Schema.Version.to_string to_} in
							Lockfile.lockfile := Some upgraded;
							Eio.Path.rename lockfile_path lock_backup_path;
							match Lockfile.write () with
							| Error err -> Error (`Lockfile err)
							| Ok () ->
								lock_backup_created := true;
								Ok ()
				else Ok ()
			in
			let* () =
				if needs_manifest_upgrade then
					Error.tag_manifest @@ begin
						Logs.app (fun m ->
							m "Upgrading %s: %a → %a"
							Manifest.filename Schema.Version.pp manifest_version Schema.Version.pp to_
						);
						let upgraded_content = Str.global_replace (Str.regexp "0\\.1\\.1") (Schema.Version.to_string to_) manifest_content in
						Eio.Path.rename manifest_path manifest_backup_path;
						Eio.Path.with_open_out ~create:(`Or_truncate 0o644) manifest_path @@ fun flow ->
						Eio.Flow.copy_string upgraded_content flow;
						manifest_backup_created := true;
						Ok ()
					end
				else Ok ()
			in
			let* () =
				match Manifest.read () with
				| Ok _ -> Logs.info (fun m -> m "Manifest verified."); Ok ()
				| Error e -> Error (`Manifest (`Parsing [`ParseError e]))
			in
			let* () =
				match Lockfile.read () with
				| Ok _ -> Logs.info (fun m -> m "Lockfile verified."); Ok ()
				| Error e -> Error (`Lockfile (`Parsing e))
			in
			cleanup_backups ();
			Ok ()
		with
		| exn ->
			Logs.err (fun m -> m "Upgrade failed: %s" (Printexc.to_string exn));
			rollback ();
			Error (`Upgrade "Failed")
