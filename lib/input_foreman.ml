(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
open Name

type error = Error.input_foreman_error

module Htbl = Saturn.Htbl

type t = (Name.t, Input.t) Htbl.t

let inputs : t =
	Htbl.create
		~hashed_type: (module struct
			type t = Name.t
			let equal = Name.equal
			let hash n = Hashtbl.hash (Name.take n)
		end)
		~min_buckets: 8
		~max_buckets: 1024
		()

let pp fmt inputs' =
	let name_map : Input.t NameMap.t =
		Htbl.to_seq inputs'
		|> Seq.fold_left
				(fun acc (name, input) -> NameMap.add name input acc)
				NameMap.empty
	in
	Fmt.pf fmt "%a" (NameMap.pp Input.pp) name_map

(* Ugly code, but *shrug* *)
let pp_for_earthlings pff =
	let hp_k_v ppf' (k, v) =
		let open Fmt in
		pf ppf' "\t%a%a %s" (styled `Blue string) k (styled `Faint string) ":" v
	in
	let hp_betupled_input ppf' (name, kind, data) =
		let open Fmt in
		pf
			ppf'
			"%a: %a@;"
			(styled `Green string)
			(Name.take name)
			(styled `Faint (parens (styled `None (styled `Yellow string))))
			kind;
		pf ppf' "%a" (list ~sep: (any "@.") hp_k_v) data;
	and betuple (input : Input.t) : Name.t * string * (string * string) list =
		let models = Input.jg_models2 input in
		let fill = Input.Template.fill ~models in
		let kind_name, kind_tuples =
			match input.kind with
			| `File f ->
				"file",
				("url", fill f.url) :: List.map (fun m -> "mirror", fill m) f.mirrors
			| `Archive a ->
				"archive",
				("url", fill a.url) :: List.map (fun m -> "mirror", fill m) a.mirrors
			| `Git g ->
				"git",
				List.concat [
					["repository", fill g.repository];
					(List.map (fun m -> "mirror", fill m) g.mirrors);
					(
						match g.reference with
						| `Branch b -> ["branch", b]
						| `Ref r -> ["ref", r]
					);
					Option.fold ~none: [] ~some: (fun d -> ["datetime", d]) g.datetime;
					["submodules", Fmt.str "%a" Fmt.bool g.submodules;
					"lfs", Fmt.str "%a" Fmt.bool g.lfs;
					];
					Option.fold ~none: [] ~some: (fun r -> ["latest-revision", r]) g.latest_revision;
				]
			| `Darcs d ->
				"darcs",
				List.concat [
					["repository", fill d.repository];
					(List.map (fun m -> ("mirror", fill m)) d.mirrors);
					(
						match d.reference with
						| `Context (`Assumed None) -> []
						| `Context (`Assumed (Some ac)) -> ["context (assumed)", ac]
						| `Context (`Stated sc) -> ["context (stated)", sc]
						| `Tag t -> [("tag", t)]
					);
					Option.fold ~none: [] ~some: (fun d -> ["datetime", d]) d.datetime;
					Option.fold ~none: [] ~some: (fun w -> ["latest-weak-hash", w]) d.latest_weak_hash;
				]
			| `Pijul p ->
				"pijul",
				List.concat [
					[("remote", fill p.remote)];
					(List.map (fun m -> "mirror", fill m) p.mirrors);
					(
						match p.reference with
						| `Channel c -> ["channel", c]
						| `State s -> ["state", s]
						| `Change c -> ["change", c]
					);
					Option.fold ~none: [] ~some: (fun d -> ["datetime", d]) p.datetime;
					Option.fold ~none: [] ~some: (fun s -> ["latest-state", s]) p.latest_state;
				]
		in
		let data_tuples : (string * string) list =
			List.concat [
				kind_tuples;
				(
					match input.latest.cmd with
					| None -> []
					| Some (cmd, cmds) ->
						let cmd_str_filled ({prog; args}: Input.Latest.Cmd.cmd) =
							List.map fill (prog :: args)
						in
						let cmds' =
							List.map cmd_str_filled (cmd :: cmds)
						and formatter =
							Fmt.list ~sep: (Fmt.any " | ") (Fmt.list ~sep: (Fmt.any " ") Fmt.string)
						in
							[("latest-cmd", Fmt.str "$ %a" formatter cmds')]
				);
				Option.fold ~none: [] ~some: (fun v -> ["latest-value", v]) input.latest.value;
				["hash-algorithm", Input.Hash.algorithm_to_string input.hash.algorithm];
				Option.fold ~none: [] ~some: (fun r -> ["hash-value", r]) input.hash.value;
				Option.fold ~none: [] ~some: (fun r -> ["hash-expected", r]) input.hash.expected;
				["frozen", Fmt.str "%a" Fmt.bool input.frozen];
			]
		in
			(input.name, kind_name, data_tuples)
	in
	Htbl.to_seq inputs
	|> Seq.fold_left (fun acc ((Name.Name name), input) -> (Name.Name name, betuple input) :: acc) []
	|> List.stable_sort (fun (name_a, _) (name_b, _) -> Name.compare name_a name_b)
	|> List.map (fun (_, s) -> s)
	|> Fmt.pf pff "%a" (Fmt.list ~sep: (Fmt.any "@.@.") hp_betupled_input)

let get name : (Input.t, error) result =
	Logs.debug (fun m -> m "Get input %a" Name.pp name);
	match Htbl.find_opt inputs name with
	| Some s -> Ok s
	| None -> Error (`Could_not_get name)

let set name input : (unit, error) result =
	Logs.debug (fun m -> m "Set input ⟨%a, %a⟩" Name.pp name Input.pp input);
	if Htbl.try_set inputs name input then
		Ok ()
	else
		Error (`Could_not_set name)

let add name input : (unit, error) result =
	Logs.debug (fun m -> m "Add input ⟨%a, %a⟩" Name.pp name Input.pp input);
	if Htbl.try_add inputs name input then
		Ok ()
	else
		Error (`Could_not_add name)

let drop name : (unit, error) result =
	Logs.debug (fun m -> m "Drop input %a" Name.pp name);
	if Htbl.try_remove inputs name then
		Ok ()
	else
		Error (`Could_not_drop name)

let to_manifest mk =
	Htbl.to_seq inputs
	|> Seq.fold_left (fun acc (name, input) -> (name, mk input) :: acc) []
	|> List.stable_sort (fun (name_a, _) (name_b, _) -> Name.compare name_a name_b)
	|> List.concat_map (fun (_, manifest_node) -> manifest_node)

let to_lockfile mk =
	Htbl.to_seq inputs
	|> Seq.fold_left
			(fun acc (name, input) -> NameMap.add name (mk input) acc)
			NameMap.empty

let unlink_or_rm_silo ~(at : [`Path of _ Eio.Path.t | `Name of Name.t]) =
	let path =
		match at with
		| `Path p -> p
		| `Name n -> Eio.Path.(Working_directory.(get () / silo_dir) / Name.take n)
	in
	match Eio.Path.kind ~follow: false path with
	| `Symbolic_link ->
		Eio.Path.unlink path;
		Logs.debug (fun m -> m "Silo: unlinked %a." Eio.Path.pp path)
	| `Directory | `Regular_file | `Socket ->
		Eio.Path.rmtree path;
		Logs.debug (fun m -> m "Silo: removed %a." Eio.Path.pp path)
	| _ ->
		()

(* TODO: make
	.silo/{name}/elotl
	.silo/{name}/milpa
	.silo/{name}/by-hash

	Where elotl (ear of maize) refers to the one using now & milpa
	(maizefield) refers to the generation. This is similar to profiles &
	could allow rollbacks & such.
*)
let make_silo_gc_root ~proc_mgr ~name ~store_path =
	let name = Name.take name in
	let silo_path = Eio.Path.(Working_directory.(get () / silo_dir)) in
	begin
		match Eio.Path.kind ~follow: false silo_path with
		| `Directory -> ()
		| `Not_found -> Working_directory.set_up_silo ()
		| _ -> failwith (Fmt.str "%a not a directory" Eio.Path.pp silo_path)
	end;
	let path = Eio.Path.(silo_path / name) in
	begin
		match Eio.Path.kind ~follow: false path with
		| `Directory -> ()
		| `Symbolic_link -> Eio.Path.unlink path;
		| `Regular_file | `Socket -> Eio.Path.rmtree path;
		| _ -> ()
	end;
	Logs.info (fun m -> m "Silo: elotl now %s ↦ %s …" name store_path);
	let elotl_path = Eio.Path.(path / "elotl") in
	unlink_or_rm_silo ~at: (`Path elotl_path);
	Eio.Process.run proc_mgr [
		"nix-store";
		"--add-root";
		Eio.Path.native_exn elotl_path;
		"--realize";
		store_path
	]

let clean_unlisted_from_silo () =
	Logs.debug (fun m -> m "Silo: cleaning unlisted …");
	let silo_path = Eio.Path.(Working_directory.(get () / silo_dir)) in
	Eio.Path.read_dir silo_path
	|> List.iter (fun nm ->
			let name = Name.make nm in
			if not (Htbl.mem inputs name) then
				unlink_or_rm_silo ~at: (`Name name)
			else
					()
		)

let cp_darcs_context ~env ~(name : Name.t) ~context =
	let (let*) = Result.bind in
	let original_path =
		if String.starts_with ~prefix: "/" context then
			Eio.Path.(Eio.Stdenv.fs env / context)
		else
			Eio.Path.(Working_directory.get () / context)
	in
	let* () = Working_directory.set_up_darcs_context_if_needed () in
	let path =
		Eio.Path.(
			Working_directory.(get () / darcs_context_dir / (Fmt.str "%s.txt" (Name.take name)))
		)
	in
	Logs.app (fun m ->
		m
			"Copying Darcs context file for %a from %a to %a …"
			Name.pp
			name
			Eio.Path.pp
			original_path
			Eio.Path.pp
			path
	);
	let () =
		Eio.Path.with_open_in original_path @@ fun input ->
		Eio.Path.with_open_out ~create: (`Or_truncate 0o644) path @@ fun output ->
		Eio.Flow.copy input output
	in
	Ok (Fmt.str "./%s/%s.txt" Working_directory.darcs_context_dir (Name.take name))

let prefetch ~env ~proc_mgr ~name () : (unit, error) result =
	Logs.app (fun m -> m "Prefetching input %a … (this may take a while)" Name.pp name);
	let open Input in
	let (let*) = Result.bind in
	let* input = get name in
	let* (new_input, new_silo_path) : Input.t * string =
		Result.map_error (fun err -> `Prefetch (input.name, err)) @@ begin
			let* latest_result = Prefetch.get_latest ~env ~proc_mgr input in
			match input.kind, latest_result with
			| `File _, `File f_data ->
				Ok (
					{input with hash = {input.hash with value = Some f_data.hash_value}},
					f_data.path
				)
			| `Archive _, `Archive a_data ->
				Ok (
					{input with hash = {input.hash with value = Some a_data.hash_value}},
					a_data.path
				)
			| `Git g, `Git g_data ->
				Ok (
					{input with
						kind =
						`Git {g with
							latest_revision = Some g_data.rev;
							datetime = g_data.datetime;
						};
						hash = {input.hash with
							algorithm = g_data.hash.algorithm;
							value = Some g_data.hash.value;
						};
					},
					g_data.path
				)
			| `Darcs d, `Darcs d_data ->
				let* reference =
					match d.reference with
					| `Context (`Assumed _) ->
						let* new_ctx =
							cp_darcs_context ~env ~name ~context: d_data.context
							|> Result.map_error (fun err -> `Darcs_context err)
						in
						Ok (`Context (`Assumed (Some new_ctx)))
					| _ ->
						Ok d.reference
				in
				Ok (
					{input with
						kind =
						`Darcs {d with
							reference;
							datetime = d_data.datetime;
							latest_weak_hash = Some d_data.weak_hash;
						};
						hash = {input.hash with
							algorithm = d_data.hash.algorithm;
							value = Some d_data.hash.value;
						};
					},
					d_data.path
				)
			| `Pijul p, `Pijul p_data ->
				Ok (
					{input with
						kind =
						`Pijul {p with
							datetime = p_data.datetime;
							latest_state = Some p_data.state;
						};
						hash = {input.hash with
							algorithm = p_data.hash.algorithm;
							value = Some p_data.hash.value;
						};
					},
					p_data.path
				)
			| _, _ -> failwith "Prefetch kind mismatch"
		end
	in
	Logs.app (fun m -> m "Prefetched %a." Name.pp input.name);
	make_silo_gc_root ~proc_mgr ~name ~store_path: new_silo_path;
	set name new_input

let run_pipeline ~sw ~proc_mgr ~(models : Input.jg_models2) cmds =
	let open Input.Latest.Cmd in
	let rec build_pipeline ?stdin = function
		| {prog; args}, [] ->
			begin
				let stdout_buf = Buffer.create 1024
				and stderr_buf = Buffer.create 1024
				in
				let stdout_sink = Eio.Flow.buffer_sink stdout_buf
				and stderr_sink = Eio.Flow.buffer_sink stderr_buf
				and cmd = List.map (Input.Template.fill ~models) (prog :: args)
				in
				try
					Eio.Process.run proc_mgr ?stdin ~stdout: stdout_sink ~stderr: stderr_sink cmd;
					Option.iter Eio.Resource.close stdin;
					(* close pipe input after last process *)
					Ok stdout_buf
				with
					| exn -> Error (exn, stderr_buf)
			end
		| {prog; args}, next :: rest ->
			begin
				let pipe_in, pipe_out = Eio.Process.pipe ~sw proc_mgr in
				let stderr_buf = Buffer.create 1024 in
				let stderr_sink = Eio.Flow.buffer_sink stderr_buf
				and cmd = List.map (Input.Template.fill ~models) (prog :: args)
				in
				try
					Eio.Process.run proc_mgr ?stdin ~stdout: pipe_out ~stderr: stderr_sink cmd;
					Eio.Resource.close pipe_out;
					(* close writer after child is spawned *)
					build_pipeline ~stdin: pipe_in (next, rest)
				with
					| exn -> Error (exn, stderr_buf)
			end
	in
	build_pipeline cmds

let get_latest ~sw ~proc_mgr input : (string option, error) result =
	match Input.latest_cmd input with
	| None ->
		Ok None
	| Some cmds ->
		let name = input.name
		and models = Input.jg_models2 input
		in
		match run_pipeline ~sw ~proc_mgr ~models cmds with
		| Error (exn, stderr) ->
			Error (`Latest_cmd_fail (name, exn, String.trim @@ Buffer.contents stderr))
		| Ok stdout_buf ->
			match String.trim @@ Buffer.contents stdout_buf with
			| "" -> Error (`Latest_cmd_empty name)
			| latest_str -> Ok (Some latest_str)

type latest_result = [
	| `LacksCmd
	| `AlreadyLatest
	| `NewLatestValue of string
]
[@@deriving show]

let lock_one ~env ~sw ~proc_mgr ~force ~name : (unit, error) result =
	Logs.info (fun m ->
		if force then m "Locking input %a …" Name.pp name
		else m "Locking input %a if needed …" Name.pp name
	);
	let (let*) = Result.bind in
	let* input = get name in
	let* () =
		match input.latest.cmd, input.latest.value, force with
		(* Only if we have a command, but no value or forced *)
		| Some _, None, _ | Some _, _, true ->
			Logs.app (fun m -> m "Fetching latest value for %a …" Name.pp name);
			begin
				match get_latest ~sw ~proc_mgr input with
				| Error err -> Error err
				| Ok None -> Ok ()
				| Ok (Some new_value) ->
					Logs.info (fun m -> m "New latest value: %a" Fmt.string new_value);
					let latest = {input.latest with value = Some new_value} in
					set name {input with latest}
			end
		| _, _, _ -> Ok ()
	in
	let needs_prefetch : bool =
		force
		|| if Option.is_none input.hash.value then
			true
		else
			match input.kind with
			| `File _ -> false
			| `Archive _ -> false
			| `Git g -> Option.is_none g.latest_revision
			| `Darcs d -> Option.is_none d.latest_weak_hash
			| `Pijul p -> Option.is_none p.latest_state
	in
	if needs_prefetch then
		prefetch ~env ~proc_mgr ~name ()
	else
		Ok ()

let lock_many ~env ~sw ~proc_mgr ~domain_count ~force ~(names : Name.t list) : (unit, error) result =
	Logs.debug (fun m ->
		m "Locking many: %a" Fmt.(brackets (list ~sep: semi Name.pp)) names
	);
	let sem = Eio.Semaphore.make domain_count
	and errors = ref []
	and any_succeed = ref false
	and result_lock = Eio.Mutex.create ()
	in
	let processes =
		List.map
			(fun name () ->
				Eio.Semaphore.acquire sem;
				Fun.protect
					~finally: (fun () -> Eio.Semaphore.release sem)
					(fun () ->
						let lock_result = lock_one ~env ~sw ~proc_mgr ~force ~name in
						Eio.Mutex.lock result_lock;
						Fun.protect
							~finally: (fun () -> Eio.Mutex.unlock result_lock)
							(fun () ->
								match lock_result with
								| Ok() -> any_succeed := true
								| Error err -> errors := err :: !errors
							)
					)
			)
			names
	in
	Eio.Fiber.all processes;
	match !any_succeed, !errors with
	| true, errs ->
		List.iter
			(fun err -> Logs.warn (fun m -> m "Couldn’t lock: %a" Error.pp_input_foreman_error err))
			errs;
		Ok ()
	| false, [err] ->
		Error err
	| false, errs ->
		let err_str =
			List.map
				(fun err -> Fmt.str "%a" Error.pp_input_foreman_error err)
				errs
		in
		Error (`Many_errors err_str)

let lock ~env ~sw ~proc_mgr ~domain_count ?(force = false) ?names () : (unit, error) result =
	match names with
	| None | Some [] ->
		let all_names =
			Htbl.to_seq inputs
			|> Seq.fold_left (fun acc (name, _) -> name :: acc) []
		in
		lock_many ~env ~sw ~proc_mgr ~domain_count ~force ~names: all_names
	| Some [name] ->
		lock_one ~env ~sw ~proc_mgr ~force ~name
	| Some names ->
		lock_many ~env ~sw ~proc_mgr ~domain_count ~force ~names

let list_stale ~env: _ ~sw ~proc_mgr ~domain_count ~names : (unit, error) result =
	Logs.debug (fun m ->
		m "Probing stale for: %a" Fmt.(brackets (list ~sep: semi Name.pp)) names
	);
	let sem = Eio.Semaphore.make domain_count
	and errors = ref []
	and stale_results = ref []
	and any_succeed = ref false
	and result_lock = Eio.Mutex.create ()
	in
	let processes =
		List.map
			(fun name () ->
				Eio.Semaphore.acquire sem;
				Fun.protect
					~finally: (fun () -> Eio.Semaphore.release sem)
					(fun () ->
						let result =
							let (let*) = Result.bind in
							let* input = get name in
							match get_latest ~sw ~proc_mgr input with
							| Error err -> Error err
							| Ok None -> Ok None
							| Ok (Some new_value) ->
								let is_outdated : string option -> bool =
									Option.fold ~none: true ~some: (Fun.compose not (String.equal new_value))
								in
								if is_outdated input.latest.value then
									Ok (Some (name, new_value))
								else
									Ok None
						in
						(* only hold the mutex for shared state updates *)
						Eio.Mutex.lock result_lock;
						Fun.protect
							~finally: (fun () -> Eio.Mutex.unlock result_lock)
							(fun () ->
								match result with
								| Ok None -> any_succeed := true
								| Ok (Some stale) ->
									any_succeed := true;
									stale_results := stale :: !stale_results
								| Error err -> errors := err :: !errors
							)
					)
			)
			names
	in
	Eio.Fiber.all processes;
	match !any_succeed, !errors with
	| true, errs ->
		List.iter
			(fun err -> Logs.warn (fun m -> m "Couldn’t refresh: %a" Error.pp_input_foreman_error err))
			errs;
		List.iter
			(fun (name, latest_value) ->
				Logs.app (fun m -> m "%a: %s" Fmt.(styled `Green string) (Name.take name) latest_value)
			)
			!stale_results;
		Ok ()
	| false, [err] -> Error err
	| false, errs ->
		let err_str = List.map (fun err -> Fmt.str "%a" Error.pp_input_foreman_error err) errs in
		Error (`Many_errors err_str)

let refresh_one ~env ~sw ~proc_mgr ~name : (unit, error) result =
	Logs.app (fun m -> m "Refreshing input %a …" Name.pp name);
	let (let*) = Result.bind in
	let* input = get name in
	let* latest_result : latest_result =
		match get_latest ~sw ~proc_mgr input with
		| Error err -> Error err
		| Ok None -> Ok `LacksCmd
		| Ok (Some(new_value : string)) ->
			Logs.info (fun m -> m "%a old latest value: %a" Name.pp name (Fmt.option ~none: (Fmt.const Fmt.string "∅") Fmt.string) input.latest.value);
			Logs.info (fun m -> m "%a new latest value: %a" Name.pp name Fmt.string new_value);
			let is_outdated : string option -> bool =
				Option.fold ~none: true ~some: (Fun.compose not (String.equal new_value))
			in
			if is_outdated input.latest.value then
				Ok (`NewLatestValue new_value)
			else
				Ok `AlreadyLatest
	in
	match latest_result with
	| `LacksCmd ->
		Logs.warn (fun m -> m "No “latest-cmd” set for %a or a default for its kind … fetching from scratch (probably wastefully)." Name.pp input.name);
		prefetch ~env ~proc_mgr ~name ()
	| `AlreadyLatest ->
		Logs.app (fun m -> m "%a already at latest; moving on." Name.pp input.name);
		Ok ()
	| `NewLatestValue new_value ->
		let latest = {input.latest with value = Some new_value} in
		let* () = set name {input with latest} in
		(* If we had a new version, then it is time to prefetch *)
		prefetch ~env ~proc_mgr ~name ()

let refresh_many ~env ~sw ~proc_mgr ~domain_count ~(names : Name.t list) : (unit, error) result =
	Logs.debug (fun m ->
		m "Refreshing many: %a" Fmt.(brackets (list ~sep: semi Name.pp)) names
	);
	let sem = Eio.Semaphore.make domain_count
	and errors = ref []
	and any_succeed = ref false
	and result_lock = Eio.Mutex.create ()
	in
	let processes =
		List.map
			(fun name () ->
				Eio.Semaphore.acquire sem;
				Fun.protect
					~finally: (fun () -> Eio.Semaphore.release sem)
					(fun () ->
						let refresh_result = refresh_one ~env ~sw ~proc_mgr ~name in
						(* only hold the mutex for the shared state update *)
						Eio.Mutex.lock result_lock;
						Fun.protect
							~finally: (fun () -> Eio.Mutex.unlock result_lock)
							(fun () ->
								match refresh_result with
								| Ok() -> any_succeed := true
								| Error err -> errors := err :: !errors
							)
					)
			)
			names
	in
	Eio.Fiber.all processes;
	match !any_succeed, !errors with
	| true, errs ->
		List.iter
			(fun err -> Logs.warn (fun m -> m "Couldn’t refresh: %a" Error.pp_input_foreman_error err))
			errs;
		Ok ()
	| false, [err] -> Error err
	| false, errs ->
		let err_str = List.map (fun err -> Fmt.str "%a" Error.pp_input_foreman_error err) errs in
		Error (`Many_errors err_str)

let refresh ~env ~sw ~proc_mgr ~domain_count ?names () : (unit, error) result =
	match names with
	| None | Some [] ->
		let all_names =
			Htbl.to_seq inputs
			|> Seq.fold_left (fun acc (name, _) -> name :: acc) []
		in
		refresh_many ~env ~sw ~proc_mgr ~domain_count ~names: all_names
	| Some [name] ->
		refresh_one ~env ~sw ~proc_mgr ~name
	| Some names ->
		refresh_many ~env ~sw ~proc_mgr ~domain_count ~names
