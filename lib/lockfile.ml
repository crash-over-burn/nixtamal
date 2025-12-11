(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
open Name

type error = Error.lockfile_error

let filename = "lock.json"

let encode_tag = Util.Jsont.encode_tag

module Uri = struct
	include Uri
	(* good enough for this *)
	let gen =
		let open QCheck.Gen in
		let a_to_z = (char_range 'a' 'z') in
		let* scheme = QCheck.Gen.oneofl ["http"; "https"; "ftp"; "sftp"] in
		let* host = string_size ~gen: a_to_z (int_bound 20) in
		let* tld = string_size ~gen: a_to_z (int_bound 5) in
		let* path_opt = option (string_size ~gen: a_to_z (int_bound 10)) in
		let uri =
			Uri.of_string @@
				Fmt.str "%s://%s.%s/%s" scheme host tld (Option.value ~default: "" path_opt)
		in
		return uri
end

module File = struct
	type t = {
		url: Uri.t;
		mirrors: Uri.t list;
	}
	[@@deriving show, eq, qcheck]

	let [@inline]to_lock
			~(models : Input.jg_models2)
			({url; mirrors; _}: Input.File.t)
			: t
		=
		let to_uri = Fun.compose Uri.of_string (Input.Template.fill ~models) in
		{
			url = to_uri url;
			mirrors = List.map to_uri mirrors;
		}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "File_lock"
			(fun url mirrors -> {url; mirrors})
		|> Object.mem "ur" Util.URI.jsont ~enc: (fun i -> i.url)
		|> Object.mem "ms" (list Util.URI.jsont) ~enc: (fun i -> i.mirrors)
		|> Object.finish
end

module Archive = struct
	type t = {
		url: Uri.t;
		mirrors: Uri.t list;
	}
	[@@deriving show, eq, qcheck]

	let [@inline]to_lock ~(models : Input.jg_models2) ({url; mirrors; _}: Input.Archive.t) : t =
		let to_uri = Fun.compose Uri.of_string (Input.Template.fill ~models) in
		{
			url = to_uri url;
			mirrors = List.map to_uri mirrors;
		}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Archive_lock"
			(fun url mirrors -> {url; mirrors})
		|> Object.mem "ur" Util.URI.jsont ~enc: (fun i -> i.url)
		|> Object.mem "ms" (list Util.URI.jsont) ~enc: (fun i -> i.mirrors)
		|> Object.finish
end

module Git = struct
	(*
	module Reference = struct
		type t = Input.Git.Reference.t
		[@@deriving show]

		let jsont : t Jsont.t =
			let open Jsont in
			let enc = function
				| `Branch brc -> encode_tag 0 string brc
				| `Ref ref -> encode_tag 1 string ref
			and dec = function
				| [|tag; value|] ->
					begin
						match Result.bind (Json.decode' uint8 tag) (function
							| 0 -> Json.decode' string value |> Result.map (fun t -> `Branch t)
							| 1 -> Json.decode' string value |> Result.map (fun c -> `Ref c)
							| n -> Error.msgf Meta.none "Unknown reference enum tag: %d" n
						) with
						| Ok v -> v
						| Error (ctx, meta, kind) -> Error.raise ctx meta kind
					end
				| _ ->
					Error.msgf Meta.none "Expected array of length 2"
			in
			map ~kind: "Git_reference_lock" ~enc ~dec (array json)
	end
	*)

	type t = {
		repository: Uri.t;
		mirrors: Uri.t list;
		(*reference: Reference.t;*)
		datetime: string option;
		submodules: bool;
		lfs: bool;
		latest_revision: string option;
	}
	[@@deriving show, eq, qcheck]

	let [@inline]to_lock
			~(models : Input.jg_models2)
			({repository; mirrors; (*reference;*) datetime; submodules; lfs; latest_revision; _}: Input.Git.t)
			: t
		=
		let to_uri = Fun.compose Uri.of_string (Input.Template.fill ~models) in
		{
			repository = to_uri repository;
			mirrors = List.map to_uri mirrors;
			(*reference;*)
			datetime;
			submodules;
			lfs;
			latest_revision;
		}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Git_lock"
			(fun repository mirrors (*reference*) datetime submodules lfs latest_revision ->
				{repository; mirrors; (*reference;*) datetime; submodules; lfs; latest_revision}
			)
		|> Object.mem "rp" Util.URI.jsont ~enc: (fun i -> i.repository)
		|> Object.mem "ms" (list Util.URI.jsont) ~enc: (fun i -> i.mirrors)
		(*|> Object.mem "rf" Reference.jsont ~enc: (fun i -> i.reference)*)
		|> Object.opt_mem "dt" string ~enc: (fun i -> i.datetime)
		|> Object.mem "sm" bool ~enc: (fun i -> i.submodules)
		|> Object.mem "lf" bool ~enc: (fun i -> i.lfs)
		|> Object.opt_mem "lr" string ~enc: (fun i -> i.latest_revision)
		|> Object.finish
end

module Darcs = struct
	module Reference = struct
		type t = Input.Darcs.Reference.t
		[@@deriving show, eq, qcheck]

		let jsont : t Jsont.t =
			let open Jsont in
			let context_jsont =
				let enc = function
					| `Assumed (Some actx) -> encode_tag 0 string actx
					| `Stated sctx -> encode_tag 1 string sctx
					(* We can’t lock without a stable reference *)
					| `Assumed None ->
						Error.msgf Meta.none "Darcs context cannot be None when locking"
				and dec = function
					| [|tag; value|] ->
						begin
							match Result.bind (Json.decode' uint8 tag) (function
								| 0 -> Json.decode' string value |> Result.map (fun a -> `Assumed (Some a))
								| 1 -> Json.decode' string value |> Result.map (fun s -> `Stated s)
								| n -> Error.msgf Meta.none "Unknown context enum tag: %d" n
							) with
							| Ok v -> v
							| Error (ctx, meta, kind) -> Error.raise ctx meta kind
						end
					| _ ->
						Error.msgf Meta.none "Expected array of length 2"
				in
				map ~kind: "Darcs_reference_context_lock" ~enc ~dec (array json)
			in
			let enc = function
				| `Context ctx -> encode_tag 0 context_jsont ctx
				| `Tag tag -> encode_tag 1 string tag
			and dec = function
				| [|tag; value|] ->
					begin
						match Result.bind (Json.decode' uint8 tag) (function
							| 0 -> Json.decode' context_jsont value |> Result.map (fun c -> `Context c)
							| 1 -> Json.decode' string value |> Result.map (fun t -> `Tag t)
							| n -> Error.msgf Meta.none "Unknown reference enum tag: %d" n
						) with
						| Ok v -> v
						| Error (ctx, meta, kind) -> Error.raise ctx meta kind
					end
				| _ ->
					Error.msgf Meta.none "Expected array of length 2"
			in
			map ~kind: "Darcs_reference_lock" ~enc ~dec (array json)
	end

	type t = {
		repository: Uri.t;
		mirrors: Uri.t list;
		datetime: string option;
		(* Darcs isn’t like the other girls; we don’t have a simple stable reference point.
		   Either the tag or context can be used. *)
		reference: Reference.t;
		latest_weak_hash: string option;
	}
	[@@deriving show, eq, qcheck]

	let [@inline]to_lock
			~(models : Input.jg_models2)
			({repository; mirrors; datetime; reference; latest_weak_hash; _}: Input.Darcs.t)
			: t
		=
		let to_uri = Fun.compose Uri.of_string (Input.Template.fill ~models) in
		{
			repository = to_uri repository;
			mirrors = List.map to_uri mirrors;
			datetime;
			reference;
			latest_weak_hash;
		}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Darcs_lock"
			(fun repository mirrors datetime reference latest_weak_hash ->
				{repository; mirrors; datetime; reference; latest_weak_hash}
			)
		|> Object.mem "rp" Util.URI.jsont ~enc: (fun i -> i.repository)
		|> Object.mem "ms" (list Util.URI.jsont) ~enc: (fun i -> i.mirrors)
		|> Object.opt_mem "dt" string ~enc: (fun i -> i.datetime)
		|> Object.mem "rf" Reference.jsont ~enc: (fun i -> i.reference)
		|> Object.opt_mem "lw" string ~enc: (fun i -> i.latest_weak_hash)
		|> Object.finish
end

module Pijul = struct
	(*
	module Reference = struct
		type t = Input.Pijul.Reference.t
		[@@deriving show]

		let jsont : t Jsont.t =
			let open Jsont in
			let enc = function
				| `Channel chn -> encode_tag 0 string chn
				| `State stt -> encode_tag 1 string stt
				| `Change chg -> encode_tag 2 string chg
			and dec = function
				| [|tag; value|] ->
					begin
						match Result.bind (Json.decode' uint8 tag) (function
							| 0 -> Json.decode' string value |> Result.map (fun c -> `Channel c)
							| 1 -> Json.decode' string value |> Result.map (fun c -> `State c)
							| 2 -> Json.decode' string value |> Result.map (fun t -> `Change t)
							| n -> Error.msgf Meta.none "Unknown reference enum tag: %d" n
						) with
						| Ok v -> v
						| Error (ctx, meta, kind) -> Error.raise ctx meta kind
					end
				| _ ->
					Error.msgf Meta.none "Expected array of length 2"
			in
			map ~kind: "Pijul_reference_lock" ~enc ~dec (array json)
	end
	*)

	type t = {
		remote: Uri.t;
		mirrors: Uri.t list;
		datetime: string option;
		(*reference: Reference.t;*)
		latest_state: string option;
	}
	[@@deriving show, eq, qcheck]

	let [@inline]to_lock
			~(models : Input.jg_models2)
			({remote; mirrors; datetime; latest_state; _}: Input.Pijul.t)
			: t
		=
		let to_uri = Fun.compose Uri.of_string (Input.Template.fill ~models) in
		{
			remote = to_uri remote;
			mirrors = List.map to_uri mirrors;
			datetime;
			latest_state;
		}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map ~kind: "Pijul_lock" (fun remote mirrors datetime (*reference*) latest_state ->
			{remote; mirrors; datetime; (*reference;*) latest_state}
		)
		|> Object.mem "rm" Util.URI.jsont ~enc: (fun i -> i.remote)
		|> Object.mem "ms" (list Util.URI.jsont) ~enc: (fun i -> i.mirrors)
		|> Object.opt_mem "dt" string ~enc: (fun i -> i.datetime)
		(* |> Object.mem "rf" Reference.jsont ~enc: (fun i -> i.reference) *)
		|> Object.mem "ls" (option string) ~enc: (fun i -> i.latest_state)
		|> Object.finish
end

module Kind = struct
	type t = [
		| `File of File.t
		| `Archive of Archive.t
		| `Git of Git.t
		| `Darcs of Darcs.t
		| `Pijul of Pijul.t
	]
	[@@deriving show, eq, qcheck]

	let to_lock ~(models : Input.jg_models2) : Input.Kind.t -> t = function
		| `File f -> `File (File.to_lock ~models f)
		| `Archive a -> `Archive (Archive.to_lock ~models a)
		| `Git g -> `Git (Git.to_lock ~models g)
		| `Darcs d -> `Darcs (Darcs.to_lock ~models d)
		| `Pijul p -> `Pijul (Pijul.to_lock ~models p)

	let jsont : t Jsont.t =
		let open Jsont in
		let enc = function
			| `File f -> encode_tag 0 File.jsont f
			| `Archive a -> encode_tag 1 Archive.jsont a
			| `Git g -> encode_tag 2 Git.jsont g
			| `Darcs d -> encode_tag 3 Darcs.jsont d
			| `Pijul p -> encode_tag 4 Pijul.jsont p
		and dec = function
			| [|tag; value|] ->
				begin
					match Result.bind (Json.decode' uint8 tag) (function
						| 0 ->
							Json.decode' File.jsont value
							|> Result.map (fun v -> `File v)
						| 1 ->
							Json.decode' Archive.jsont value
							|> Result.map (fun v -> `Archive v)
						| 2 ->
							Json.decode' Git.jsont value
							|> Result.map (fun v -> `Git v)
						| 3 ->
							Json.decode' Darcs.jsont value
							|> Result.map (fun v -> `Darcs v)
						| 4 ->
							Json.decode' Pijul.jsont value
							|> Result.map (fun v -> `Pijul v)
						| n ->
							Error.msgf Meta.none "Unknown reference enum tag: %d" n
					) with
					| Ok v -> v
					| Error (ctx, meta, kind) -> Error.raise ctx meta kind
				end
			| _ ->
				Error.msgf Meta.none "Expected array of length 2"
		in
		map ~kind: "Input_kind" ~enc ~dec (array json)
end

module Hash = struct
	type algorithm = Input.Hash.algorithm
	[@@deriving show, eq, qcheck]

	let algorithm_jsont =
		let gen_algo i =
			Input.Hash.algorithm_of_enum i
			|> Option.map (fun al -> (Input.Hash.algorithm_to_string_lower al, al))
		in
		Jsont.enum
			~kind: "Hash_algorithm_lock"
			~cmp: Input.Hash.compare_algorithm
			List.(init (Input.Hash.max_algorithm + 1) gen_algo |> filter_map Fun.id)

	type t = {
		algorithm: algorithm;
		value: string option;
	}
	[@@deriving show, eq, qcheck]

	let [@inline]to_lock ({algorithm; value; _}: Input.Hash.t) : t =
		{algorithm; value}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Hash_lock"
			(fun algorithm value -> {algorithm; value})
		|> Object.mem "al" algorithm_jsont ~enc: (fun o -> o.algorithm)
		|> Object.mem "vl" (option string) ~enc: (fun o -> o.value)
		|> Object.finish
end

module Input' = struct
	type t = {
		kind: Kind.t;
		hash: Hash.t;
		latest_value: string option;
	}
	[@@deriving show, eq, qcheck]

	let [@inline]to_lock ~(models : Input.jg_models2) (input : Input.t) : t = {
		kind = Kind.to_lock ~models input.kind;
		hash = Hash.to_lock input.hash;
		latest_value = input.latest.value;
	}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map ~kind: "Input_lock" (fun kind hash latest_value -> {kind; hash; latest_value})
		|> Object.mem "kd" Kind.jsont ~enc: (fun i -> i.kind)
		|> Object.mem "ha" Hash.jsont ~enc: (fun i -> i.hash)
		|> Object.opt_mem "lv" string ~enc: (fun i -> i.latest_value)
		|> Object.finish
end

type inputs = Input'.t NameMap.t
[@@deriving show, eq, qcheck]

type t = {
	version: string;
	inputs: inputs;
}
[@@deriving show, eq, qcheck]

let lockfile : t option ref = ref None

let jsont : t Jsont.t =
	let open Jsont in
	Object.map ~kind: "Lockfile" (fun version inputs -> {version; inputs})
	|> Object.mem "v" Jsont.string ~enc: (fun i -> i.version)
	|> Object.mem "i" (NameMap.jsont ~kind: "Input" Input'.jsont) ~enc: (fun i -> i.inputs)
	|> Object.finish

let make ?(version = "0.1.0") () =
	Logs.info (fun m -> m "Making lockfile @@ version:%s …" version);
	let inputs =
		Input_foreman.to_lockfile (fun input ->
			let models = Input.jg_models2 input in
			Input'.to_lock ~models input
		);
	in
	let doc : t = {version; inputs} in
	Logs.debug (fun m -> m "New JSON lockfile:@;%a@." pp doc);
	lockfile := Some doc;
	Ok doc

let exists () : bool =
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Eio.Path.is_file filepath

let read () =
	let (let*) = Result.bind in
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Logs.info (fun m -> m "Reading lockfile @@ %a …" Eio.Path.pp filepath);
	if Eio.Path.is_file filepath then
		begin
			let* lock =
				Eio.Path.with_open_in filepath @@ fun flow ->
				Util.Jsont.of_flow jsont flow
			in
			Ok (Some lock)
		end
	else
		begin
			Logs.warn (fun m -> m "Lockfile missing @@ %a. Consider running the lock command." Eio.Path.pp filepath);
			Ok None
		end

let write ?(create = `Or_truncate 0o644) () : (unit, error) result =
	let (let*) = Result.bind in
	let* lock =
		match !lockfile with
		| Some lock -> Ok lock
		| None -> make ()
	in
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Logs.app (fun m -> m "Writing lockfile @@ %s …" filename);
	let* result =
		Eio.Path.with_open_out ~create filepath @@ fun flow ->
		(* TODO: Util.Jsont.to_flow_piset jsont lock flow *)
		Util.Jsont.to_flow jsont lock flow
		|> Result.map_error (fun err -> `Serializing err)
	in
	Logs.app (fun m -> m "Lockfile written.");
	Ok result
