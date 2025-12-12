(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
open Name

let pp_exn ppf exn =
	Fmt.pf ppf "%s" (Printexc.to_string exn)

type manifest_error = [
	| `Parsing of KDL.Valid.err list
	| `Not_set_up
	| `File_already_exists
]
[@@deriving show]

type lockfile_error = [
	| `Parsing of string
	| `Serializing of string
]
[@@deriving show]

type prefetch_method = [
	| `URL
	| `Git
	| `Darcs
	| `Pijul
]
[@@deriving show]

type prefetch_error = [
	| `Empty_output of prefetch_method
	| `JSON_parsing of prefetch_method * string
	| `Darcs_context of string
	| `RunException of prefetch_method * exn * string
]
[@@deriving show]

type input_foreman_error = [
	| `Could_not_add of Name.t
	| `Could_not_drop of Name.t
	| `Could_not_get of Name.t
	| `Could_not_set of Name.t
	| `Latest_cmd_empty of Name.t
	| `Latest_cmd_fail of Name.t * exn * string
	| `Prefetch of Name.t * prefetch_error
	| `Pool_exception of exn
	(* FIXME: string list *)
	| `Many_errors of string list
]
[@@deriving show]

type error = [
	| `Manifest of manifest_error
	| `Lockfile of lockfile_error
	| `Version_mismatch of string * string
	| `Input_foreman of input_foreman_error
]
[@@deriving show]

let [@inline]tag_manifest (res : ('a, manifest_error) result) =
	Result.map_error (fun err -> `Manifest err) res

let [@inline]tag_lockfile (res : ('a, lockfile_error) result) =
	Result.map_error (fun err -> `Lockfile err) res

let [@inline]tag_input_foreman res =
	Result.map_error (fun err -> `Input_foreman err) res

let pp ppf = function
	| `Manifest err ->
		Fmt.(pf ppf "%a" pp_manifest_error err)
	| `Lockfile err ->
		Fmt.(pf ppf "%a" pp_lockfile_error err)
	| `Version_mismatch (mnfst, lock) ->
		Fmt.pf ppf "Version mismatch: Manifest@@%s & Lockfile@@%s" mnfst lock
	| `Input_foreman (`CouldNotAdd name) ->
		Fmt.pf ppf "Could not set %a" Name.pp name
