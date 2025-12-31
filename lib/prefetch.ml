(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
type error = Error.prefetch_error

let run_and_gather ~proc_mgr ~(proc_env : string array) ?(buffer_size = 1024) method' cmd : (string, error) result =
	Logs.debug (fun m -> m "Running %a cmd: %a" Error.pp_prefetch_method method' (Fmt.list ~sep: Fmt.sp Fmt.string) cmd);
	let (let*) = Result.bind in
	let stdout_buf = Buffer.create buffer_size
	and stderr_buf = Buffer.create buffer_size
	in
	let stdout_sink = Eio.Flow.buffer_sink stdout_buf
	and stderr_sink = Eio.Flow.buffer_sink stderr_buf
	in
	let* () =
		try
			Eio.Process.run proc_mgr ~env: proc_env ~stdout: stdout_sink ~stderr: stderr_sink cmd;
			Ok ()
		with
			| exn ->
				Error (`Run_exception (method', exn, String.trim (Buffer.contents stdout_buf)))
	in
	match String.trim (Buffer.contents stdout_buf) with
	| "" -> Error (`Empty_output method')
	| stdout ->
		Logs.debug (fun m -> m "Command %a output: %s" Error.pp_prefetch_method method' stdout);
		Ok stdout

module Hash = struct
	type t = {
		algorithm: Input.Hash.algorithm;
		value: string;
	}
	[@@deriving show]

	let make_from_opts blake3 sha256 sha512 =
		match blake3, sha256, sha512 with
		| Some value, None, None -> {algorithm = Input.Hash.BLAKE3; value}
		| None, Some value, None -> {algorithm = Input.Hash.SHA256; value}
		| None, None, Some value -> {algorithm = Input.Hash.SHA512; value}
		| None, None, None ->
			Jsont.Error.msgf Jsont.Meta.none "Missing supported hash type"
		| _, _, _ ->
			Jsont.Error.msgf Jsont.Meta.none "Multiple supported hash types; expecting just 1"

	let add_jsont_case obj =
		let open Jsont in
		obj
		|> Object.opt_mem "blake3" string
		|> Object.opt_mem "sha256" string
		|> Object.opt_mem "sha512" string
end

module File = struct
	type t = {
		path: string;
		hash_value: string;
	}

	(* env can assert it is a path *)
	let of_stdout ?env (stdout : string) : t option =
		match String.split_on_char '\n' (String.trim stdout), env with
		| hash_value :: path :: _, None ->
			Some {path; hash_value}
		| hash_value :: path :: _, Some env' when Option.is_some (Eio.Path.native (Eio.Path.(Eio.Stdenv.fs env' / path))) ->
			Some {path; hash_value}
		| _ ->
			None

	let latest_cmd (f : Input.File.t) ~(hash_algo : string) ~models =
		let url = URI.of_string (Input.Template.fill f.url ~models) in
		[
			"nix-prefetch-url";
			URI.to_string url;
			"--print-path";
			"--type";
			hash_algo;
		]

	let get_latest ~env ~proc_mgr ~proc_env (f : Input.File.t) ~(hash_algo : string) ~models =
		let (let*) = Result.bind in
		let method' = `URL in
		let* stdout = run_and_gather ~proc_mgr ~proc_env method' (latest_cmd f ~hash_algo ~models) in
		match of_stdout ~env stdout with
		| None -> Error (`Bad_output (method', stdout))
		| Some t -> Ok t
end

module Archive = struct
	type t = {
		path: string;
		hash_value: string;
	}

	(* env can assert it is a path *)
	let of_stdout ?env (stdout : string) : t option =
		match String.split_on_char '\n' (String.trim stdout), env with
		| hash_value :: path :: _, None ->
			Some {path; hash_value}
		| hash_value :: path :: _, Some env' when Option.is_some (Eio.Path.native (Eio.Path.(Eio.Stdenv.fs env' / path))) ->
			Some {path; hash_value}
		| _ ->
			None

	let latest_cmd (a : Input.Archive.t) ~(hash_algo : string) ~models =
		let url = URI.of_string (Input.Template.fill a.url ~models) in
		[
			"nix-prefetch-url";
			URI.to_string url;
			"--print-path";
			"--unpack";
			"--type";
			hash_algo;
		]

	let get_latest ~env ~proc_mgr ~proc_env (a : Input.Archive.t) ~(hash_algo : string) ~models =
		let (let*) = Result.bind in
		let method' = `URL in
		let* stdout = run_and_gather ~proc_mgr ~proc_env method' (latest_cmd a ~hash_algo ~models) in
		match of_stdout ~env stdout with
		| None -> Error (`Bad_output (method', stdout))
		| Some t -> Ok t
end

module Git = struct
	type t = {
		datetime: string option;
		path: string;
		rev: string;
		hash: Hash.t;
	}
	[@@deriving make, show]

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Prefetch_Git"
			(fun path datetime rev blake3 sha256 sha512 ->
				let hash = Hash.make_from_opts blake3 sha256 sha512 in
				make ~path ?datetime ~rev ~hash ()
			)
		|> Object.mem "path" string ~enc: (fun i -> i.path)
		|> Object.opt_mem "date" string ~enc: (fun i -> i.datetime)
		|> Object.mem "rev" string ~enc: (fun i -> i.rev)
		|> Hash.add_jsont_case
		|> Object.finish

	let latest_cmd (g : Input.Git.t) ~models =
		let cmd = [
			"nix-prefetch-git";
			"--no-deepClone";
			"--quiet";
			"--url";
			URI.to_string (URI.of_string (Input.Template.fill g.repository ~models));
		]
		in
		List.concat [
			cmd;
			(
				match g.reference with
				| `Branch b -> ["--branch-name"; b]
				| `Ref r -> ["--rev"; r]
			);
			if g.submodules then ["--fetch-submodules"] else [];
			if g.lfs then ["--fetch-lfs"] else [];
		]

	let get_latest ~proc_mgr ~proc_env (g : Input.Git.t) ~models =
		let (let*) = Result.bind in
		let method' = `Git in
		let* stdout = run_and_gather ~proc_mgr ~proc_env method' (latest_cmd g ~models) in
		Jsont_bytesrw.decode_string jsont stdout
		|> Result.map_error (fun err -> `JSON_parsing (method', err))
end

module Darcs = struct
	type t = {
		path: string;
		datetime: string option;
		context: string;
		weak_hash: string;
		hash: Hash.t;
	}
	[@@deriving make, show]

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Prefetch_Darcs"
			(fun path datetime context weak_hash blake3 sha256 sha512 ->
				let hash = Hash.make_from_opts blake3 sha256 sha512 in
				make ~path ?datetime ~context ~weak_hash ~hash ()
			)
		|> Object.mem "path" string ~enc: (fun i -> i.path)
		|> Object.opt_mem "date" string ~enc: (fun i -> i.datetime)
		|> Object.mem "context" string ~enc: (fun i -> i.context)
		|> Object.mem "weak-hash" string ~enc: (fun i -> i.weak_hash)
		|> Hash.add_jsont_case
		|> Object.finish

	let latest_cmd (d : Input.Darcs.t) ~models =
		(* formatter looks ugly so doing cmd = cmd @ […] *)
		let cmd = ["nix-prefetch-darcs"] in
		let cmd =
			match d.reference with
			| `Context (`Assumed _) -> cmd
			| `Context (`Stated sc) -> cmd @ ["--context"; sc]
			| `Tag t -> cmd @ ["--tag"; t]
		in
		cmd @ [
			URI.to_string (URI.of_string (Input.Template.fill d.repository ~models));
		]

	let get_latest ~proc_mgr ~proc_env (d : Input.Darcs.t) ~models =
		let (let*) = Result.bind in
		let method' = `Darcs in
		let* stdout = run_and_gather ~proc_mgr ~proc_env method' (latest_cmd d ~models) in
		Jsont_bytesrw.decode_string jsont stdout
		|> Result.map_error (fun err -> `JSON_parsing (method', err))
end

module Pijul = struct
	type t = {
		path: string;
		datetime: string option;
		state: string;
		hash: Hash.t
	}
	[@@deriving make, show]

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Prefetch_Pijul"
			(fun path datetime state blake3 sha256 sha512 ->
				let hash = Hash.make_from_opts blake3 sha256 sha512 in
				make ~path ?datetime ~state ~hash ()
			)
		|> Object.mem "path" string ~enc: (fun i -> i.path)
		|> Object.opt_mem "date" string ~enc: (fun i -> i.datetime)
		|> Object.mem "state" string ~enc: (fun i -> i.state)
		|> Hash.add_jsont_case
		|> Object.finish

	let latest_cmd (p : Input.Pijul.t) ~models =
		let cmd = [
			"nix-prefetch-pijul";
			"--remote";
			URI.to_string (URI.of_string (Input.Template.fill p.remote ~models));
		]
		in
		cmd @
			match p.reference with
			| `Change c -> ["--change"; c]
			| `Channel c -> ["--channel"; c]
			| `State s -> ["--state"; s]

	let get_latest ~proc_mgr ~proc_env (p : Input.Pijul.t) ~models =
		let (let*) = Result.bind in
		let method' = `Pijul in
		let* stdout = run_and_gather ~proc_mgr ~proc_env method' (latest_cmd p ~models) in
		Jsont_bytesrw.decode_string jsont stdout
		|> Result.map_error (fun err -> `JSON_parsing (method', err))
end

type prefetch_kind_result = (
	[
		| `File of File.t
		| `Archive of Archive.t
		| `Git of Git.t
		| `Darcs of Darcs.t
		| `Pijul of Pijul.t
	],
	error
) result

let get_latest ~env ~proc_mgr (input : Input.t) : prefetch_kind_result =
	let hash_algo = Input.Hash.algorithm_to_string_lower input.hash.algorithm in
	let proc_env =
		let unix_env = Unix.environment () in
		Array.append unix_env [|"NIX_HASH_ALGO=" ^ hash_algo|]
	in
	let models = Input.jg_models2 input in
	match input.kind with
	| `File file ->
		File.get_latest ~env ~proc_mgr ~proc_env file ~hash_algo ~models
		|> Result.map (fun f -> `File f)
	| `Archive archive ->
		Archive.get_latest ~env ~proc_mgr ~proc_env archive ~hash_algo ~models
		|> Result.map (fun a -> `Archive a)
	| `Git git ->
		Git.get_latest ~proc_mgr ~proc_env git ~models
		|> Result.map (fun g -> `Git g)
	| `Darcs darcs ->
		Darcs.get_latest ~proc_mgr ~proc_env darcs ~models
		|> Result.map (fun d -> `Darcs d)
	| `Pijul pijul ->
		Pijul.get_latest ~proc_mgr ~proc_env pijul ~models
		|> Result.map (fun p -> `Pijul p)
