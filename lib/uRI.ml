(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(* extend & fix naming for ocaml-uri *)
include Uri

let jsont : t Jsont.t =
	Jsont.string
	|> Jsont.map ~kind: "URI" ~dec: of_string ~enc: to_string

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
