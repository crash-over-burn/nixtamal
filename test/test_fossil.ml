(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
open Alcotest
open Nixtamal

let suite =
	let t = Input.Template.make in
	[
		test_case "Manifest Fossil reference check-in from KDL" `Quick (fun () ->
			let reference =
				let kdl =
					{|check-in "abc123"|}
					|> KDL.of_string
					|> Result.get_ok
				in
				match Manifest.Fossil.Reference.codec.of_kdl kdl with
				| Ok ref -> ref
				| Error err -> failwith Fmt.(str "%a" (list ~sep: semi KDL.Valid.pp_err) err)
			in
			check
				(testable Input.Fossil.Reference.pp Input.Fossil.Reference.equal)
				"check-in is decoded as Fossil reference"
				(`Checkin "abc123")
				reference
		);
		test_case "Manifest Fossil input roundtrip" `Quick (fun () ->
			let fossil =
				Manifest.Fossil.make
					~repository:(t "https://example.org/src.fossil")
					~reference:(`Branch "trunk")
			in
			let roundtrip =
				fossil
				|> Manifest.Fossil.codec.to_kdl
				|> Manifest.Fossil.codec.of_kdl
				|> Result.get_ok
			in
			check
				(testable Manifest.Fossil.pp Manifest.Fossil.equal)
				"Fossil KDL codec is shape-preserving"
				fossil
				roundtrip
		);
		test_case "Lockfile Fossil kind roundtrip" `Quick (fun () ->
			let input_kind =
				Input.make_kind_fossil
					~repository:(t "https://example.org/src.fossil")
					~reference:(`Tag "v1.0")
					~date:"2026-01-01T00:00:00Z"
					~latest_checkin:"abc123"
					()
			in
			let models = Input.jg_models2 @@ Input.make ~name:(Name.Name.make "fossil") ~kind:input_kind () in
			let lock_kind = Lockfile.Kind.to_lock ~models input_kind in
			let decoded =
				lock_kind
				|> Jsont.Json.encode Lockfile.Kind.jsont
				|> Result.get_ok
				|> Jsont.Json.decode Lockfile.Kind.jsont
				|> Result.get_ok
			in
			check
				(testable Lockfile.Kind.pp Lockfile.Kind.equal)
				"Fossil lockfile kind JSON codec is shape-preserving"
				lock_kind
				decoded;
			match lock_kind with
			| `Fossil fossil_lock ->
				check int "Fossil mirrors are always empty" 0 (List.length fossil_lock.mirrors);
				check (option string) "Fossil lock datetime" (Some "2026-01-01T00:00:00Z") fossil_lock.datetime;
				check (option string) "Fossil lock latest_checkin" (Some "abc123") fossil_lock.latest_checkin
			| _ -> fail "Expected Fossil lock kind"
		);
	]
