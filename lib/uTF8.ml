(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(* Extending Camomile *)
include Camomile.UTF8

let equal a b = compare a b = 0

let pp ppf str = Fmt.pf ppf "%s" str

let gen_uchar : Camomile.UChar.t QCheck.Gen.t =
	let open QCheck.Gen in
	let* i =
		oneof [
			int_range 0 0xD7FF;
			int_range 0xE000 0x10FFFF;
		];
	in
	return (Camomile.UChar.of_int i)

let gen : t QCheck.Gen.t =
	let open QCheck.Gen in
	let* size = int_bound 40 in
	let* chars = list_size (return size) gen_uchar in
	let buf = Buf.create size in
	List.iter (fun u -> Buf.add_char buf u) chars;
	return (Buf.contents buf)
