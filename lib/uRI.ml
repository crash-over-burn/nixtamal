(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(* extend & fix naming for ocaml-uri *)
include Uri

let jsont : t Jsont.t =
	Jsont.string
	|> Jsont.map ~kind: "URI" ~dec: of_string ~enc: to_string

(* Validate URI for security concerns *)
let acceptable_schemes = ["http"; "https"; "ftp"; "sftp"; "file"; "ssh"; "git"; "darcs"; "pijul"; "fossil"]

let is_acceptable_scheme scheme =
	List.mem (String.lowercase_ascii scheme) acceptable_schemes

let contains_substring s substr =
	let re = Str.regexp_string substr in
	try
		ignore (Str.search_forward re s 0);
		true
	with Not_found -> false

let has_path_traversal uri =
	let path_str = path uri in
	contains_substring path_str ".." && (
		contains_substring path_str "/../" ||
		contains_substring path_str "\\..\\" ||
		String.starts_with ~prefix:"../" path_str ||
		String.ends_with ~suffix:"/.." path_str
	)

let validate uri =
	match scheme uri with
	| Some scheme when is_acceptable_scheme scheme ->
		if has_path_traversal uri then
			Error (`Path_traversal (path uri))
		else
			Ok ()
	| Some scheme ->
		Error (`Invalid_scheme scheme)
	| None ->
		Error (`Invalid_scheme "missing")

(* good enough URI generation for now for this *)
let gen =
	let open QCheck.Gen in
	let a_to_z = (char_range 'a' 'z') in
	let* scheme = QCheck.Gen.oneof_list ["http"; "https"; "ftp"; "sftp"] in
	let* host = string_size ~gen: a_to_z (int_bound 20) in
	let* tld = string_size ~gen: a_to_z (int_bound 5) in
	let* path_opt = option (string_size ~gen: a_to_z (int_bound 10)) in
	let uri =
		of_string @@
			Fmt.str "%s://%s.%s/%s" scheme host tld (Option.value ~default: "" path_opt)
	in
	return uri
