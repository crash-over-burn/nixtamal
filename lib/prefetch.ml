(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
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

module Git = struct
	type t = {
		datetime: string option;
		rev: string;
		hash: Hash.t;
	}
	[@@deriving make, show]

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Prefetch_Git"
			(fun datetime rev blake3 sha256 sha512 ->
				let hash = Hash.make_from_opts blake3 sha256 sha512 in
				make ?datetime ~rev ~hash ()
			)
		|> Object.opt_mem "date" string ~enc: (fun i -> i.datetime)
		|> Object.mem "rev" string ~enc: (fun i -> i.rev)
		|> Hash.add_jsont_case
		|> Object.finish
end

module Darcs = struct
	type t = {
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
			(fun datetime context weak_hash blake3 sha256 sha512 ->
				let hash = Hash.make_from_opts blake3 sha256 sha512 in
				make ?datetime ~context ~weak_hash ~hash ()
			)
		|> Object.opt_mem "date" string ~enc: (fun i -> i.datetime)
		|> Object.mem "context" string ~enc: (fun i -> i.context)
		|> Object.mem "weak-hash" string ~enc: (fun i -> i.weak_hash)
		|> Hash.add_jsont_case
		|> Object.finish
end

module Pijul = struct
	type t = {
		datetime: string option;
		state: string;
		hash: Hash.t
	}
	[@@deriving make, show]

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Prefetch_Pijul"
			(fun datetime state blake3 sha256 sha512 ->
				let hash = Hash.make_from_opts blake3 sha256 sha512 in
				make ?datetime ~state ~hash ()
			)
		|> Object.opt_mem "date" string ~enc: (fun i -> i.datetime)
		|> Object.mem "state" string ~enc: (fun i -> i.state)
		|> Hash.add_jsont_case
		|> Object.finish
end
