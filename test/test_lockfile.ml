(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
open Alcotest
open Nixtamal

let write_file path content =
	Eio.Path.with_open_out ~create:(`Or_truncate 0o644) path @@ fun flow ->
	Eio.Flow.copy_string content flow

let setup_workdir ~env name =
	let cwd = Eio.Stdenv.cwd env in
	let dir = Eio.Path.(cwd / "_build" / "tests" / name) in
	(
		match Eio.Path.kind ~follow:true dir with
		| `Not_found -> ()
		| _ -> Eio.Path.rmtree dir
	);
	Eio.Path.mkdirs ~perm:0o755 dir;
	Working_directory.set ~directory:dir;
	dir

let write_minimal_manifest ~version dir =
	let content = Fmt.str {|version "%s"
inputs {}
|} version in
	write_file Eio.Path.(dir / Manifest.filename) content

let suite =
	[
		test_case "Lockfile auto-creation from manifest" `Quick (fun () ->
			Eio_main.run @@ fun env ->
			let dir = setup_workdir ~env "lockfile-auto-create" in
			write_minimal_manifest ~version:"0.1.1" dir;
			Lockfile.lockfile := None;
			let res = read_manifest_and_lockfile () in
			check bool "manifest and lock read succeeds" true (Result.is_ok res);
			check bool "lockfile was created in memory" true (Option.is_some !Lockfile.lockfile)
		);
		test_case "Lockfile write/read roundtrip" `Quick (fun () ->
			Eio_main.run @@ fun env ->
			let dir = setup_workdir ~env "lockfile-roundtrip" in
			write_minimal_manifest ~version:"0.1.1" dir;
			let name = Name.Name.make "lockfile-roundtrip-input" in
			let input =
				Input.make
					~name
					~kind:(Input.make_kind_file ~url:(Input.Template.make "https://example.org/archive.tar.gz") ())
					()
			in
			ignore (Input_foreman.add name input);
			let made =
				match Lockfile.make ~version:"0.1.1" () with
				| Ok lock -> lock
				| Error err -> failf "Failed to make lockfile: %s" (Fmt.str "%a" Error.pp_lockfile_error err)
			in
			check bool "write succeeds" true (Result.is_ok (Lockfile.write ()));
			let reread =
				match Lockfile.read () with
				| Ok (Some lock) -> lock
				| Ok None -> fail "Expected lockfile on disk"
				| Error err -> failf "Failed to read lockfile: %s" err
			in
			check
				(testable Lockfile.pp Lockfile.equal)
				"serialized lockfile roundtrips"
				made
				reread
		);
	]
