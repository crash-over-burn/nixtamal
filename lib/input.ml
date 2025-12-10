(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
open Name

type jg_models2 = string -> Jingoo.Jg_types.tvalue

module Template = struct
	type t =
		Template of string
	[@@unboxed]
	[@@deriving show]

	let [@inline]make t = Template t
	let [@inline]take (Template t) = t
	let [@inline]fill ~(models : jg_models2) tpl =
		Jingoo.Jg_template2.from_string ~models (take tpl)
end

module Latest = struct
	module Cmd = struct
		type 'a non_empty_list =
				('a * 'a list)
		[@@deriving show]

		type cmd = {
			prog: Template.t;
			args: Template.t list;
		}
		[@@deriving show, make]

		type t = cmd non_empty_list
		[@@deriving show]

		let (~$) x = (x, [])
		let (|:) (x, xs) x' = (x, x' :: xs)
		let (@) (x, xs) (y, ys) = (x, xs @ y :: ys)
	end

	type t = {
		cmd: Cmd.t option;
		value: string option;
	}
	[@@deriving show, make]
end

(* KINDS **********************************************************************)

module File = struct
	type t = {
		url: Template.t;
		mirrors: Template.t list;
	}
	[@@deriving show, make]
end

module Archive = struct
	type t = {
		url: Template.t;
		mirrors: Template.t list;
	}
	[@@deriving show, make]
end

module Git = struct
	module Reference = struct
		type t = [
			| `Branch of string
			| `Ref of string
		]
		[@@deriving show]
	end

	type t = {
		repository: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
		datetime: string option; (* ISO 8601 RFC 3339 *)
		submodules: bool; [@default false]
		lfs: bool; [@default false]
		latest_revision: string option;
	}
	[@@deriving show, make]

	let default_latest_cmd git : Latest.Cmd.t =
		let open Latest.Cmd in
		let git_ls_remote flag value : t =
			let m = Latest.Cmd.make_cmd in
			let t = Template.make in
			~$(m ~prog: (t "git") ~args: [t "ls-remote"; t flag; git.repository; t "--refs"; t value] ())
			|: (m ~prog: (t "cut") ~args: [t "-f1"] ())
		in
		match git.reference with
		| `Branch b -> git_ls_remote "--branches" b
		| `Ref r -> git_ls_remote "--heads" r
end

module Darcs = struct
	module Reference = struct
		type t = [
			| `Context of [`Assumed of string option | `Stated of string]
			| `Tag of string
		]
		[@@deriving show]
	end

	type t = {
		repository: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
		datetime: string option; (* ISO 8601 RFC 3339 *)
		latest_weak_hash: string option;
	}
	[@@deriving show, make]

	let pp fmt t = Fmt.pf fmt "%s" (show t)
end

module Pijul = struct
	module Reference = struct
		type t = [
			| `Channel of string
			| `State of string
			| `Change of string
		]
		[@@deriving show]
	end

	type t = {
		remote: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
		datetime: string option; (* ISO 8601 RFC 3339 *)
		latest_state: string option;
	}
	[@@deriving show, make]
end

module Hash = struct
	type algorithm =
		| SHA256
		| SHA512
		| BLAKE3
	[@@deriving enum, eq, ord, show]

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
		value: string option;
		(* used to assert in fetching for manually-updated pins *)
		expected: string option;
	}
	[@@deriving show, make]
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
	[@@deriving show]
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
[@@deriving show, make]

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
	try Hashtbl.find hashtbl needle with Not_found -> Tnull

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

	let mk_latest ~reference ?latest_value () : Latest.t =
		let mk_latest_cmd ~flag ~arg : Latest.Cmd.t =
			let open Latest.Cmd in
			let m = Latest.Cmd.make_cmd in
			let t = Template.make in
			~$(m ~prog: (t "git") ~args: [t "ls-remote"; t flag; default_git_repository; t "--refs"; t arg] ())
			|: (m ~prog: (t "cut") ~args: [t "-f1"] ())
		in
		{
			cmd = begin
				match reference with
				| `Ref r -> Some (mk_latest_cmd ~flag: "--heads" ~arg: r);
				| `Branch b -> Some (mk_latest_cmd ~flag: "--branches" ~arg: b);
			end;
			value = latest_value;
		}

	let make_archive ?(reference = `Ref default_ref) ?latest_value () =
		let latest = mk_latest ~reference ?latest_value () in
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
