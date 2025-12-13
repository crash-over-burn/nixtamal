(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
open Name

type jg_models2 = string -> Jingoo.Jg_types.tvalue

module Template = struct
	type t =
		Template of UTF8.t
	[@@unboxed]
	[@@deriving show, eq, qcheck]

	let [@inline]make t = Template t
	let [@inline]take (Template t) = t
	let [@inline]fill ~(models : jg_models2) tpl =
		Jingoo.Jg_template2.from_string ~models (take tpl)
end

module Latest = struct
	module Cmd = struct
		type cmd = {
			prog: Template.t;
			args: Template.t list;
		}
		[@@deriving show, eq, make, qcheck]

		type t = cmd Util.Non_empty_list.t
		[@@deriving show, eq, qcheck]

		let (~$) x = (x, [])
		let (|:) (x, xs) x' = (x, x' :: xs)
		let (@) (x, xs) (y, ys) = (x, xs @ y :: ys)
	end

	type t = {
		cmd: Cmd.t option;
		value: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]
end

(* KINDS **********************************************************************)

module File = struct
	type t = {
		url: Template.t;
		mirrors: Template.t list;
	}
	[@@deriving show, eq, make, qcheck]
end

module Archive = struct
	type t = {
		url: Template.t;
		mirrors: Template.t list;
	}
	[@@deriving show, eq, make, qcheck]
end

module Git = struct
	module Reference = struct
		type t = [
			| `Branch of UTF8.t
			| `Ref of UTF8.t
		]
		[@@deriving show, eq, qcheck]
	end

	type t = {
		repository: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
		datetime: UTF8.t option; (* ISO 8601 RFC 3339 *)
		submodules: bool; [@default false]
		lfs: bool; [@default false]
		latest_revision: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]

	let default_latest_cmd git : Latest.Cmd.t =
		let open Latest.Cmd in
		let t = Template.make in
		let git_ls_remote (ls_remote_args : Template.t list) : t =
			let args = t "ls-remote" :: git.repository :: ls_remote_args in
			~${prog = t "git"; args}
			|: {prog = t "cut"; args = [t "-f1"]}
		in
		match git.reference with
		| `Branch b -> git_ls_remote [t "--branches"; t b]
		| `Ref r -> git_ls_remote [t "--refs"; t r]
end

module Darcs = struct
	module Reference = struct
		type context_grounds = [
			| `Assumed of UTF8.t option
			| `Stated of UTF8.t
		]
		[@@deriving show, eq, qcheck]

		type t = [
			| `Context of context_grounds
			| `Tag of UTF8.t
		]
		[@@deriving show, eq, qcheck]
	end

	type t = {
		repository: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
		datetime: UTF8.t option; (* ISO 8601 RFC 3339 *)
		latest_weak_hash: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]
end

module Pijul = struct
	module Reference = struct
		type t = [
			| `Channel of UTF8.t
			| `State of UTF8.t
			| `Change of UTF8.t
		]
		[@@deriving show, eq, qcheck]
	end

	type t = {
		remote: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
		datetime: UTF8.t option; (* ISO 8601 RFC 3339 *)
		latest_state: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]
end

module Hash = struct
	type algorithm =
		| SHA256
		| SHA512
		| BLAKE3
	[@@deriving enum, eq, ord, show, qcheck]

	let algorithm_to_string = function
		| SHA256 -> "SHA256"
		| SHA512 -> "SHA512"
		| BLAKE3 -> "BLAKE3"

	let algorithm_to_string_lower =
		Fun.compose String.lowercase_ascii algorithm_to_string

	let algorithm_of_string = function
		| "SHA256" | "sha256" -> Some SHA256
		| "SHA512" | "sha512" -> Some SHA512
		| "BLAKE3" | "blake3" -> Some BLAKE3
		| _ -> None

	(* many of the builtin fetchers may only work with SHA256 *)
	let default_algorithm = SHA256

	type t = {
		algorithm: algorithm;
			[@default default_algorithm]
		(* None is for not yet calculated *)
		value: UTF8.t option;
		(* used to assert in fetching for manually-updated pins *)
		expected: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]
end

(* INPUT *******************************************************************)

module Kind = struct
	type t = [
		| `File of File.t
		| `Archive of Archive.t
		| `Git of Git.t
		| `Darcs of Darcs.t
		| `Pijul of Pijul.t
	]
	[@@deriving show, eq, qcheck]
end

let make_kind_file ~url ?mirrors () =
	`File (File.make ~url ?mirrors ())

let make_kind_archive ~url ?mirrors () =
	`Archive (Archive.make ~url ?mirrors ())

let make_kind_darcs ~repository ?mirrors ~reference ?latest_weak_hash () =
	`Darcs (Darcs.make ~repository ?mirrors ~reference ?latest_weak_hash ())

let make_kind_pijul ~remote ?mirrors ~reference ?latest_state () =
	`Pijul (Pijul.make ~remote ?mirrors ~reference ?latest_state ())

let make_kind_git ~repository ?mirrors ~reference ?latest_revision ?submodules ?lfs () =
	`Git (Git.make ~repository ?mirrors ~reference ?latest_revision ?submodules ?lfs ())

type t = {
	name: Name.t;
	kind: Kind.t;
	(* This is use to override or provide a command to get the latest change or
		revision or timestamp or whatever. *)
	latest: Latest.t; [@default Latest.make ()]
	hash: Hash.t; [@default Hash.make ()]
	frozen: bool; [@default false]
}
[@@deriving show, eq, make, qcheck]

let latest_cmd (input : t) : Latest.Cmd.t option =
	match input.latest.cmd with
	| None ->
		(
			match input.kind with
			| `Git g -> Some (Git.default_latest_cmd g)
			(* Would be nice if other tools did a better job letting you query the
			remote repository directly, but that isn’t where we are *)
			| _ -> None
		)
	| Some cmd -> Some cmd

(* JINGOO MODELS **************************************************************)

let jg_models2 (input : t) (needle : string) : Jingoo.Jg_types.tvalue =
	let open Jingoo.Jg_types in
	let opt_count = Option.fold ~none: 0 ~some: (Fun.const 1) in
	(* presupplied with global values *)
	let make_hashtbl (further_size : int) : (string, tvalue) Hashtbl.t =
		let size = 1 + opt_count input.latest.value in
		let htbl = Hashtbl.create (size + further_size) in
		Hashtbl.add htbl "name" (Tstr (Name.take input.name));
		Option.iter (fun v -> Hashtbl.add htbl "cmd_value" (Tstr v)) input.latest.value;
		htbl
	in
	let hashtbl =
		match input.kind with
		| `File _ ->
			make_hashtbl 0
		| `Archive _ ->
			make_hashtbl 0
		| `Git g ->
			begin
				let htbl = make_hashtbl 5 in
				(
					match g.reference with
					| `Branch b -> Hashtbl.add htbl "branch" (Tstr b)
					| `Ref r -> Hashtbl.add htbl "ref" (Tstr r)
				);
				Option.iter (fun d -> Hashtbl.add htbl "datetime" (Tstr d)) g.datetime;
				Hashtbl.add htbl "lfs" (Tbool g.lfs);
				Hashtbl.add htbl "submodules" (Tbool g.submodules);
				Option.iter
					(fun r ->
						List.iter (fun key -> Hashtbl.add htbl key (Tstr r)) ["rev"; "revision"]
					)
					g.latest_revision;
				htbl
			end
		| `Darcs d ->
			begin
				let htbl = make_hashtbl 2 in
				(
					match d.reference with
					| `Context (`Stated sc) ->
						Hashtbl.add htbl "context" (Tstr sc)
					| `Context (`Assumed ac) ->
						Option.iter (fun c -> Hashtbl.add htbl "context" (Tstr c)) ac
					| `Tag t ->
						Hashtbl.add htbl "tag" (Tstr t)
				);
				Option.iter (fun d -> Hashtbl.add htbl "datetime" (Tstr d)) d.datetime;
				Option.iter (fun w -> Hashtbl.add htbl "weak_hash" (Tstr w)) d.latest_weak_hash;
				htbl
			end
		| `Pijul p ->
			begin
				let htbl = make_hashtbl 2 in
				(
					match p.reference with
					| `Channel c -> Hashtbl.add htbl "channel" (Tstr c)
					| `State s -> Hashtbl.add htbl "state" (Tstr s)
					| `Change c -> Hashtbl.add htbl "change" (Tstr c)
				);
				Option.iter (fun d -> Hashtbl.add htbl "datetime" (Tstr d)) p.datetime;
				Option.iter (fun s -> Hashtbl.add htbl "state" (Tstr s)) p.latest_state;
				htbl
			end
	in
	match Hashtbl.find_opt hashtbl needle with
	| Some value -> value
	| None -> Tnull

(* NIXPKGS ********************************************************************)

(* Nixpkgs is so critical & valuable to the Nix ecosystem that it gets its own
	special treatment; it is also *required* to get access to many of the
	fetchers *)
module Nixpkgs = struct
	let name = Name.make "nixpkgs"

	let default_git_repository = Template.make "https://github.com/NixOS/nixpkgs.git"

	(* NOTE: "refs/heads/nixpkgs-unstable" is probably good enough for your
 		project, but defaulting to nixos-unstable since it is ‘safer’, requiring
 		that all the NixOS tests pass *)
	let default_ref = "refs/heads/nixos-unstable"

	let default_hash = Hash.make ~algorithm: Hash.SHA256 ()

	let known_git_mirrors : Template.t list =
		List.map Template.make [
			"https://mirrors.tuna.tsinghua.edu.cn/git/nixpkgs.git"
		]

	let make_latest ~reference ?latest_value () : Latest.t =
		let open Latest.Cmd in
		let t = Template.make in
		let git_ls_remote (ls_remote_args : Template.t list) : t =
			let args = t "ls-remote" :: default_git_repository :: ls_remote_args in
			~${prog = t "git"; args}
			|: {prog = t "cut"; args = [t "-f1"]}
		in
		{
			cmd = begin
				match reference with
				| `Ref r -> Some (git_ls_remote [t "--refs"; t r])
				| `Branch b -> Some (git_ls_remote [t "--branches"; t b])
			end;
			value = latest_value;
		}

	let make_archive ?(reference = `Ref default_ref) ?latest_value () =
		let latest = make_latest ~reference ?latest_value () in
		let url =
			Template.make "https://github.com/NixOS/nixpkgs/archive/{{cmd_value}}.tar.gz"
		in
		let kind = make_kind_archive ~url () in
		make ~name ~kind ~latest ~hash: default_hash ()

	(* The TUNA mirror is a Git mirror, so normalize on Git *)
	let make_git_with_known_mirrors
			?(extra_mirrors = [])
			?(reference = `Ref default_ref)
			?latest_revision
			?submodules
			?lfs
			()
		=
		let kind =
			make_kind_git
				~repository: default_git_repository
				~mirrors: (known_git_mirrors @ extra_mirrors)
				~reference
				?latest_revision
				?submodules
				?lfs
				()
		in
		make ~name ~kind ~hash: default_hash ()
end
