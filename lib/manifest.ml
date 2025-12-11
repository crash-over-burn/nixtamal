(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
open Name

type error = Error.manifest_error

(* TODO: modify the KDL tree in-place to preserve comments… this is probably a
	hefty refactor since I would possibly need to drop the codec type for
	Kdl.lens (tho the concepts of “lens” & “codec” are similar) *)
let filename = "manifest.kdl"

let default_hash_algorithm : Input.Hash.algorithm option ref = ref None

module Template = struct
	include Input.Template

	let to_arg ?annot tpl =
		Kdl.arg ?annot (`String (take tpl))

	let of_child ~name kdl =
		let open Util.KDL.L in
		let open Util.KDL.Valid in
		ll @@ Result.map make (kdl.@(child name // arg 0 // string_value))

	let of_mirrors kdl =
		let open Util.KDL.L in
		let open Util.KDL.Valid in
		ll @@
			match kdl.@(child "mirrors" // args // each string_value) with
			| Ok ms -> Ok (List.map make ms)
			| Error (`Not_found ("mirrors", _)) -> Ok []
			| Error err -> Error err
end

module File = struct
	type t = {
		url: Template.t;
		mirrors: Template.t list;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({url; mirrors; _}: Input.File.t) : t =
		make ~url ~mirrors ()

	let [@inline]of_manifest ({url; mirrors}: t) : Input.File.t =
		Input.File.make ~url ~mirrors ()

	let codec : t Util.KDL.codec = {
		to_kdl = (fun file ->
			let open Kdl in
			let nodes =
				if List.is_empty file.mirrors then
						[]
				else
						[node "mirrors" ~args: (List.map Template.to_arg file.mirrors) []]
			in
			let nodes =
				node "url" ~args: [Template.to_arg file.url] [] :: nodes
			in
				[node "file" nodes]
		);
		of_kdl = (fun kdl ->
			let open Util.KDL.L in
			let open Util.KDL.Valid in
			let* file = ll @@ kdl.@(node "file") in
			let+ url = Template.of_child ~name: "url" file
			and+ mirrors = Template.of_mirrors file
			in
				{url; mirrors}
		);
	}
end

module Archive = struct
	type t = {
		url: Template.t;
		mirrors: Template.t list;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({url; mirrors; _}: Input.Archive.t) : t =
		make ~url ~mirrors ()

	let [@inline]of_manifest ({url; mirrors}: t) : Input.Archive.t =
		Input.Archive.make ~url ~mirrors ()

	let codec : t Util.KDL.codec = {
		to_kdl = (fun archive ->
			let open Kdl in
			let url =
				node "url" ~args: [Template.to_arg archive.url] [];
			and nodes =
				if List.is_empty archive.mirrors then
						[]
				else
						[node "mirrors" ~args: (List.map Template.to_arg archive.mirrors) []]
			in
			let nodes = url :: nodes in
				[node "archive" nodes]
		);
		of_kdl = (fun kdl ->
			let open Util.KDL.L in
			let open Util.KDL.Valid in
			let* archive = ll @@ kdl.@(node "archive") in
			let+ url = Template.of_child ~name: "url" archive
			and+ mirrors = Template.of_mirrors archive
			in
				{url; mirrors}
		);
	}
end

module Git = struct
	module Reference = struct
		type t = Input.Git.Reference.t
		[@@deriving show, eq, qcheck]

		let codec : t Util.KDL.codec = {
			to_kdl = (fun ref ->
				let open Kdl in
				match ref with
				| `Branch b -> [Kdl.node "branch" ~args: [arg (`String b)] []]
				| `Ref r -> [Kdl.node "ref" ~args: [arg (`String r)] []]
			);
			of_kdl = (fun kdl ->
				let open Util.KDL.L in
				let open Util.KDL.Valid in
				let node_names = ["branch"; "ref"]
				and branch = ll @@ kdl.@(node "branch" // arg 0 // string_value)
				and ref = ll @@ kdl.@(node "ref" // arg 0 // string_value)
				in
				match branch, ref with
				| Ok b, Error _ -> Ok (`Branch b)
				| Error _, Ok r -> Ok (`Ref r)
				| Error _, Error _ -> Error [`OneRequired node_names]
				| _, _ -> Error [`OnlyOneOf node_names]
			);
		}
	end

	type t = {
		repository: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
		submodules: bool; [@default false]
		lfs: bool; [@default false]
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({repository; mirrors; reference; submodules; lfs; _}: Input.Git.t) : t =
		make ~repository ~mirrors ~reference ~submodules ~lfs ()

	let [@inline]of_manifest ({repository; mirrors; reference; submodules; lfs}: t) : Input.Git.t =
		Input.Git.make ~repository ~mirrors ~reference ~submodules ~lfs ()

	let codec : t Util.KDL.codec = {
		to_kdl = (fun git ->
			let open Kdl in
			let repository =
				node "repository" ~args: [Template.to_arg git.repository] []
			and nodes =
				if git.lfs then [node "lfs" []] else []
			in
			let nodes =
				if git.submodules then node "submodules" [] :: nodes else nodes
			in
			let nodes =
				match git.reference with
				| `Branch b -> node "branch" ~args: [arg (`String b)] [] :: nodes
				| `Ref r -> node "ref" ~args: [arg (`String r)] [] :: nodes
			in
			let nodes =
				if List.is_empty git.mirrors then
					nodes
				else
					node "mirrors" ~args: (List.map Template.to_arg git.mirrors) [] :: nodes
			in
			let nodes = repository :: nodes in
				[node "git" nodes]
		);
		of_kdl = (fun kdl ->
			let open Util.KDL.L in
			let open Util.KDL.Valid in
			let* git = ll @@ kdl.@(node "git") in
			let+ repository = Template.of_child ~name: "repository" git
			and+ mirrors = Template.of_mirrors git
			and+ reference = Reference.codec.of_kdl git.children
			and+ submodules =
				ll @@
					match git.@(child "submodules") with
					| Ok sms ->
						begin
							match sms.@(arg 0 // bool_value) with
							| Ok smb -> Ok smb
							| Error (`Missing_index 0) -> Ok true
							| Error err -> Error err
						end
					| Error (`Not_found ("submodules", _)) -> Ok false
					| Error err -> Error err
			and+ lfs =
				ll @@
					match git.@(child "lfs") with
					| Ok sms ->
						begin
							match sms.@(arg 0 // bool_value) with
							| Ok smb -> Ok smb
							| Error (`Missing_index 0) -> Ok true
							| Error err -> Error err
						end
					| Error (`Not_found ("lfs", _)) -> Ok false
					| Error err -> Error err
			in
				{repository; mirrors; reference; submodules; lfs}
		);
	}
end

module Darcs = struct
	module Reference = struct
		type context_grounds = [
			| `Assumed of UTF8.t option
			| `Stated of UTF8.t
		]
		[@@deriving show, eq]

		let gen_context_grounds =
			let open QCheck.Gen in
			oneof [
				return (`Assumed None);
				map (fun s -> `Stated s) UTF8.gen;
			]

		type t = [
			| `Context of context_grounds
			| `Tag of UTF8.t
		]
		[@@deriving show, eq, qcheck]

		let codec : t Util.KDL.codec = {
			to_kdl = (fun ref ->
				let open Kdl in
				match ref with
				| `Context (`Stated sc) -> [Kdl.node "context" ~args: [arg (`String sc)] []]
				| `Context (`Assumed _) -> []
				| `Tag t -> [Kdl.node "tag" ~args: [arg (`String t)] []]
			);
			of_kdl = (fun kdl ->
				let open Util.KDL.L in
				let open Util.KDL.Valid in
				let node_names = ["tag"; "context"]
				and context = ll @@ kdl.@(node "context" // arg 0 // string_value)
				and tag = ll @@ kdl.@(node "tag" // arg 0 // string_value)
				in
				match context, tag with
				| Ok c, Error _ -> Ok (`Context (`Stated c))
				| Error _, Ok t -> Ok (`Tag t)
				| Error _, Error _ -> Ok (`Context (`Assumed None))
				| _, _ -> Error [`OnlyOneOf node_names]
			);
		}
	end

	type t = {
		repository: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({repository; mirrors; reference; _}: Input.Darcs.t) : t =
		make ~repository ~mirrors ~reference ()

	let [@inline]of_manifest ({repository; mirrors; reference}: t) : Input.Darcs.t =
		Input.Darcs.make ~repository ~mirrors ~reference ()

	let codec : t Util.KDL.codec = {
		to_kdl = (fun darcs ->
			let open Kdl in
			let repository =
				node "repository" ~args: [Template.to_arg darcs.repository] []
			and nodes =
				Reference.codec.to_kdl darcs.reference
			in
			let nodes =
				if List.is_empty darcs.mirrors then
					nodes
				else
					node "mirrors" ~args: (List.map Template.to_arg darcs.mirrors) [] :: nodes
			in
			let nodes = repository :: nodes in
				[node "darcs" nodes]
		);
		of_kdl = (fun kdl ->
			let open Util.KDL.L in
			let open Util.KDL.Valid in
			let* darcs = ll @@ kdl.@(node "darcs") in
			let+ repository = Template.of_child ~name: "repository" darcs
			and+ mirrors = Template.of_mirrors darcs
			and+ reference = Reference.codec.of_kdl darcs.children
			in
				{repository; mirrors; reference}
		);
	}
end

module Pijul = struct
	module Reference = struct
		type t = Input.Pijul.Reference.t
		[@@deriving show, eq, qcheck]

		let codec : t Util.KDL.codec = {
			to_kdl = (fun ref ->
				let open Kdl in
				match ref with
				| `Channel c -> [Kdl.node "channel" ~args: [arg (`String c)] []]
				| `State s -> [Kdl.node "state" ~args: [arg (`String s)] []]
				| `Change c -> [Kdl.node "change" ~args: [arg (`String c)] []]
			);
			of_kdl = (fun kdl ->
				let open Util.KDL.L in
				let open Util.KDL.Valid in
				let node_names = ["channel"; "state"; "change"]
				and channel = ll @@ kdl.@(node "channel" // arg 0 // string_value)
				and state = ll @@ kdl.@(node "state" // arg 0 // string_value)
				and change = ll @@ kdl.@(node "change" // arg 0 // string_value)
				in
				match channel, state, change with
				| Ok c, Error _, Error _ -> Ok (`Channel c)
				| Error _, Ok s, Error _ -> Ok (`State s)
				| Error _, Error _, Ok c -> Ok (`Change c)
				| Error _, Error _, Error _ -> Error [`OneRequired node_names]
				| _, _, _ -> Error [`OnlyOneOf node_names]
			);
		}
	end

	type t = {
		remote: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({remote; mirrors; reference; _}: Input.Pijul.t) : t =
		make ~remote ~mirrors ~reference ()

	let [@inline]of_manifest ({remote; mirrors; reference}: t) : Input.Pijul.t =
		Input.Pijul.make ~remote ~mirrors ~reference ()

	let codec : t Util.KDL.codec = {
		to_kdl = (fun pijul ->
			let open Kdl in
			let remote =
				node "remote" ~args: [Template.to_arg pijul.remote] []
			and nodes =
				Reference.codec.to_kdl pijul.reference
			in
			let nodes =
				if List.is_empty pijul.mirrors then
					nodes
				else
					node "mirrors" ~args: (List.map Template.to_arg pijul.mirrors) [] :: nodes
			in
			let nodes = remote :: nodes in
				[node "pijul" nodes]
		);
		of_kdl = (fun kdl ->
			let open Util.KDL.L in
			let open Util.KDL.Valid in
			let* pijul = ll @@ kdl.@(node "pijul") in
			let+ remote = Template.of_child ~name: "remote" pijul
			and+ mirrors = Template.of_mirrors pijul
			and+ reference = Reference.codec.of_kdl pijul.children
			in
				{remote; mirrors; reference}
		);
	}
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

	let to_manifest : Input.Kind.t -> t = function
		| `File f -> `File (File.to_manifest f)
		| `Archive a -> `Archive (Archive.to_manifest a)
		| `Git g -> `Git (Git.to_manifest g)
		| `Darcs d -> `Darcs (Darcs.to_manifest d)
		| `Pijul p -> `Pijul (Pijul.to_manifest p)

	let of_manifest : t -> Input.Kind.t = function
		| `File f -> `File (File.of_manifest f)
		| `Archive a -> `Archive (Archive.of_manifest a)
		| `Git g -> `Git (Git.of_manifest g)
		| `Darcs d -> `Darcs (Darcs.of_manifest d)
		| `Pijul p -> `Pijul (Pijul.of_manifest p)

	let codec : t Util.KDL.codec = {
		to_kdl = (function
			| `File f -> File.codec.to_kdl f
			| `Archive a -> Archive.codec.to_kdl a
			| `Git g -> Git.codec.to_kdl g
			| `Darcs d -> Darcs.codec.to_kdl d
			| `Pijul p -> Pijul.codec.to_kdl p
		);
		of_kdl = (fun kdl ->
			let kind_names = ["file"; "archive"; "git"; "darcs"; "pijul"] in
			match File.codec.of_kdl kdl,
			Archive.codec.of_kdl kdl,
			Git.codec.of_kdl kdl,
			Darcs.codec.of_kdl kdl,
			Pijul.codec.of_kdl kdl with
			| Ok file, Error _, Error _, Error _, Error _ ->
				Ok (`File file)
			| Error _, Ok archive, Error _, Error _, Error _ ->
				Ok (`Archive archive)
			| Error _, Error _, Ok git, Error _, Error _ ->
				Ok (`Git git)
			| Error _, Error _, Error _, Ok darcs, Error _ ->
				Ok (`Darcs darcs)
			| Error _, Error _, Error _, Error _, Ok pijul ->
				Ok (`Pijul pijul)
			| Error _, Error _, Error _, Error _, Error _ ->
				Error [`OneRequired kind_names]
			| _, _, _, _, _ ->
				Error [`OnlyOneOf kind_names]
		);
	}
end

module Hash = struct
	type t = {
		algorithm: Input.Hash.algorithm; [@default Input.Hash.default_algorithm]
		expected: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({algorithm; expected; _}: Input.Hash.t) : t =
		make ~algorithm ?expected ()

	let [@inline]of_manifest ({algorithm; expected}: t) : Input.Hash.t =
		Input.Hash.make ~algorithm ?expected ()

	let codec : t Util.KDL.codec = {
		to_kdl = (fun hash ->
			let open Kdl in
			let props =
				match hash.expected with
				| None -> []
				| Some exp_hash ->
					["expected", (None, `String exp_hash)]
			in
			let props =
				let algo_str = Input.Hash.algorithm_to_string hash.algorithm in
				("algorithm", (None, `String algo_str)) :: props
			in
				[node "hash" ~props []]
		);
		of_kdl = (fun kdl ->
			let open Util.KDL.L in
			let open Util.KDL.Valid in
			let* hash = ll @@ kdl.@(node "hash") in
			let+ algorithm : Input.Hash.algorithm option =
				match hash.@(prop "algorithm") with
				| Ok algo ->
					begin
						let* algo_val = ll @@ algo.@(string_value) in
						match Input.Hash.algorithm_of_string algo_val with
						| Some av -> Ok (Some av)
						| None ->
							let len : int = Input.Hash.max_algorithm - Input.Hash.min_algorithm + 1
							and algo_str (i : int) : UTF8.t =
								i + Input.Hash.min_algorithm
								|> Input.Hash.algorithm_of_enum
								|> Option.get
								|> Input.Hash.algorithm_to_string
							in
							let algo_str_list : UTF8.t list = List.init len algo_str in
							Logs.err (fun m ->
								m
									"Got hash algorithm “%s”, but exepected one of %a"
									algo_val
									Fmt.(brackets (list ~sep: semi string))
									algo_str_list
							);
							Error [`OneRequired algo_str_list]
					end
				| Error (`Missing_prop "algorithm") -> ll @@ Ok !default_hash_algorithm
				| Error err -> ll @@ Error err

			and+ expected : UTF8.t option =
				ll @@
					match hash.@(prop "expected") with
					| Ok exp -> map Option.some @@ exp.@(string_value)
					| Error (`Missing_prop "expected") -> Ok None
					| Error err -> Error err
			in
			make ?algorithm ?expected ()
		);
	}
end

module Input' = struct
	type t = {
		name: Name.t;
		kind: Kind.t;
		latest_cmd: Input.Latest.Cmd.t option;
		hash: Hash.t;
		frozen: bool; [@default false]
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest (input : Input.t) : t = {
		name = input.name;
		kind = Kind.to_manifest input.kind;
		latest_cmd = input.latest.cmd;
		hash = Hash.to_manifest input.hash;
		frozen = input.frozen;
	}

	let [@inline]of_manifest (mnfst : t) : Input.t = {
		name = mnfst.name;
		kind = Kind.of_manifest mnfst.kind;
		latest = Input.Latest.make ?cmd: mnfst.latest_cmd ();
		hash = Hash.of_manifest mnfst.hash;
		frozen = mnfst.frozen;
	}

	let codec : t Util.KDL.node_codec = {
		to_node = (fun input ->
			let open Kdl in
			let props =
				if input.frozen then
						[("frozen", arg (`Bool true))]
				else
						[]
			and kind = Kind.codec.to_kdl input.kind
			and hash = Hash.codec.to_kdl input.hash
			and latest_cmd =
				match input.latest_cmd with
				| None -> []
				| Some (exec, pipes) ->
					let cmd_args ({prog; args}: Input.Latest.Cmd.cmd) =
						List.map (Template.to_arg) (prog :: args)
					in
					let nodes =
						List.map (fun pcmd -> node "|" ~args: (cmd_args pcmd) []) pipes
					in
					let nodes =
						node "$" ~args: (cmd_args exec) [] :: nodes
					in
						[node "latest-cmd" nodes]
			in
			let nodes = kind @ hash @ latest_cmd in
			node (Name.take input.name) ~props nodes
		);
		of_node = (fun input ->
			let open Util.KDL.L in
			let open Util.KDL.Valid in
			let strip_quotes str =
				let len = String.length str in
				if len > 1 && str.[0] = '"' && str.[len - 1] = '"' then
					String.sub str 1 (len - 2)
				else
					str
			in
			let+ name =
				ll @@ input.@(node_name)
				|> Result.map Name.make
			and+ latest_cmd : Input.Latest.Cmd.t option =
				let extract_cmds (node : Kdl.node) =
					if List.is_empty node.props then
						let string_cmd (_a, v) : Template.t =
							Fmt.to_to_string Kdl.pp_value v
							|> strip_quotes
							|> Template.make
						in
						match List.map string_cmd node.args with
						| [] -> Error [`InvalidLatestCmd "Empty command"]
						| prog :: args -> Ok ({prog; args}: Input.Latest.Cmd.cmd)
					else
						Error [`InvalidLatestCmd "Props aren’t supported (yet?); you probably meant to add straight quotes (for example “\"--foo=bar\"”)."]
				in
				let rec extract_all_cmds acc = function
					| [] -> acc
					| cmd_list :: cmds_list ->
						let acc' =
							match acc, extract_cmds cmd_list with
							| Error errs, Ok _ -> Error errs
							| Ok _, Error errs -> Error errs
							| Ok ok_acc, Ok cmd -> Ok (ok_acc @ [cmd])
							| Error errs, Error errs' -> Error (errs @ errs')
						in
						extract_all_cmds acc' cmds_list
				in
				match input.@(child "latest-cmd") with
				| Error _ ->
					Ok None
				| Ok lcmd_node ->
					let+ exec =
						let* exec' = ll @@ lcmd_node.@(child ~nth: 0 "$") in
						extract_cmds exec'
					and+ pipes =
						match ll @@ lcmd_node.@(child_many "|") with
						| Ok ps -> extract_all_cmds (Ok []) ps
						| Error [`Not_found ("|", _)] -> Ok []
						| Error err -> Error err
					in
					Some (exec, pipes)
			and+ hash =
				match Hash.codec.of_kdl input.children with
				| Ok h ->
					Ok h
				| Error (`Not_found ("hash", _) :: []) ->
					Ok (Hash.make ?algorithm: !default_hash_algorithm ())
				| Error errs ->
					Error errs
			and+ kind =
				Kind.codec.of_kdl input.children
			and+ frozen =
				ll @@
					match input.@(prop "frozen") with
					| Ok f -> f.@(bool_value)
					| Error`Missing_prop "frozen" -> Ok false
					| Error err -> Error err
			in
				{name; kind; latest_cmd; hash; frozen}
		);
	}
end

type t = {
	version: UTF8.t;
	inputs: Input'.t list;
}
[@@deriving show, eq, make, qcheck]

let document_to_t (doc : Kdl.t) : t Util.KDL.Valid.t =
	let open Util.KDL.L in
	let open Util.KDL.Valid in
	let* manifest_default_hash_algorithm : Input.Hash.algorithm option =
		match ll @@ doc.@(node "default_hash_algorithm" // arg 0 // string_value) with
		| Ok dha ->
			begin
				match Input.Hash.algorithm_of_string dha with
				| Some ha -> Ok (Some ha)
				| None -> Error [`InvalidHashAlgorithm dha]
			end
		| Error (`Not_found ("default_hash_algorithm", _) :: []) ->
			Ok None
		| Error errs ->
			Error errs
	in
	let () = default_hash_algorithm := manifest_default_hash_algorithm in
	let+ version : UTF8.t =
		ll @@ doc.@(node "version" // arg 0 // string_value)
	and+ inputs : Input'.t list =
		(* TODO: a lens would mean this could use `each` *)
		let rec get_inputs acc = function
			| [] -> acc
			| (input : Kdl.node) :: inputs_tail ->
				let acc' =
					match acc, Input'.codec.of_node input with
					| Error errs, Ok _ -> Error errs
					| Ok _, Error errs -> Error errs
					| Ok ok_acc, Ok src -> Ok (ok_acc @ [src])
					| Error errs, Error errs' -> Error (errs @ errs')
				in
				get_inputs acc' inputs_tail
		in
		Result.bind
			(ll @@ doc.@(node "inputs" // children))
			(get_inputs (Ok []))
	in
	make ~version ~inputs ()

let manifest : Kdl.t option ref = ref None

let exists () : bool =
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Eio.Path.is_file filepath

let read () =
	let (let*) = Result.bind in
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Logs.info (fun m -> m "Reading manifest @@ %a …" Eio.Path.pp filepath);
	let* kdl =
		Eio.Path.with_open_in filepath @@ fun flow ->
		Util.KDL.of_flow flow
	in
	let () = manifest := Some kdl in
	Ok kdl

let make ?(version = "0.1.0") () =
	Logs.app (fun m -> m "Making manifest file @@ version:%s" version);
	let open Kdl in
	let doc = [
		node "version" ~args: [arg (`String version)] [];
		node "inputs" (
			Input_foreman.to_manifest (fun s ->
				let open Input' in
					[codec.to_node (to_manifest s)]
			)
		);
	]
	in
	Logs.debug (fun m -> m "New KDL doc:@;%a@." Kdl.pp doc);
	manifest := Some doc

let write () : (unit, error) result =
	let (let*) = Result.bind in
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	let* mnfst =
		match !manifest with
		| Some m -> Ok m
		| None -> Error `Not_set_up
	in
	Logs.app (fun m -> m "Writing manifest @@ %s …" filename);
	let result =
		Eio.Path.with_open_out ~create: (`Exclusive 0o644) filepath @@ fun flow ->
		let banner = [
			Cstruct.of_string "// ┏┓╻+╻ ╱┏┳┓┏┓┏┳┓┏┓╻\n";
			Cstruct.of_string "// ┃┃┃┃┗━┓╹┃╹┣┫┃┃┃┣┫┃   Read the manpage:\n";
			Cstruct.of_string "// ╹┗┛╹╱ ╹ ╹ ╹╹╹ ╹╹╹┗┛  $ man nixtamal-manifest\n";
		]
		in
		Eio.Flow.write flow banner;
		Util.KDL.to_flow flow mnfst;
		Eio.Flow.write flow ([Cstruct.of_string "\n"])
	in
	Logs.app (fun m -> m "Manifest written.");
	Ok result
