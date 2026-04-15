(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
open Alcotest
open Nixtamal

let write_file path content =
	Eio.Path.with_open_out ~create:(`Or_truncate 0o644) path @@ fun flow ->
	Eio.Flow.copy_string content flow

let read_file path =
	Eio.Path.with_open_in path @@ fun flow ->
	let buf = Eio.Buf_read.of_flow flow ~max_size: max_int in
	Eio.Buf_read.take_all buf

let has_substring haystack needle =
	try
		ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
		true
	with Not_found -> false

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

let write_minimal_lockfile ~version dir =
	let content = Fmt.str {|{"v":"%s","i":{}}|} version in
	write_file Eio.Path.(dir / Lockfile.filename) content

let suite =
	[
		test_case "Upgrade backup path naming" `Quick (fun () ->
			check string "backup extension" "manifest.kdl.bak" (backup_path "manifest.kdl")
		);
			test_case "Upgrade dry-run keeps files untouched" `Quick (fun () ->
			Eio_main.run @@ fun env ->
			let dir = setup_workdir ~env "upgrade-dry-run" in
			write_minimal_manifest ~version:"0.1.1" dir;
			write_minimal_lockfile ~version:"0.1.1" dir;
			Lockfile.lockfile := None;
			let res = upgrade ~to_:Schema.Version.V0_2_0 ~dry_run:true () in
			check bool "dry-run succeeds" true (Result.is_ok res);
			let manifest_content = read_file Eio.Path.(dir / Manifest.filename) in
			check bool "manifest stays old version" true (has_substring manifest_content "0.1.1");
			check bool "manifest backup not created" false (Eio.Path.is_file Eio.Path.(dir / backup_path Manifest.filename));
			check bool "lock backup not created" false (Eio.Path.is_file Eio.Path.(dir / backup_path Lockfile.filename))
		);
		test_case "Upgrade rewrites versions and cleans backups" `Quick (fun () ->
			Eio_main.run @@ fun env ->
			let dir = setup_workdir ~env "upgrade-success" in
			write_minimal_manifest ~version:"0.1.1" dir;
			write_minimal_lockfile ~version:"0.1.1" dir;
			Lockfile.lockfile := None;
			let res = upgrade ~to_:Schema.Version.V0_2_0 () in
			check bool "upgrade succeeds" true (Result.is_ok res);
			let manifest_content = read_file Eio.Path.(dir / Manifest.filename) in
			check bool "manifest version upgraded" true (has_substring manifest_content "0.2.0");
			let lock_version =
				match Lockfile.read () with
				| Ok (Some lock) -> lock.version
				| Ok None -> fail "Lockfile missing after upgrade"
				| Error err -> failf "Failed to read lockfile: %s" err
			in
			check string "lockfile version upgraded" "0.2.0" lock_version;
			check bool "manifest backup cleaned" false (Eio.Path.is_file Eio.Path.(dir / backup_path Manifest.filename))
		);
		test_case "Upgrade aborts on version mismatch" `Quick (fun () ->
			Eio_main.run @@ fun env ->
			let dir = setup_workdir ~env "upgrade-version-mismatch" in
			write_minimal_manifest ~version:"0.1.1" dir;
			write_minimal_lockfile ~version:"0.1.1" dir;
			Lockfile.lockfile := None;
			let res = upgrade ~from:Schema.Version.V0_2_0 ~to_:Schema.Version.V0_2_0 () in
			check bool "upgrade fails" true (Result.is_error res);
			let lock_version =
				match Lockfile.read () with
				| Ok (Some lock) -> lock.version
				| _ -> fail "Lockfile should still be present after abort"
			in
			check string "lockfile unchanged" "0.1.1" lock_version;
			check bool "manifest backup not created" false (Eio.Path.is_file Eio.Path.(dir / backup_path Manifest.filename));
			check bool "lockfile backup not created" false (Eio.Path.is_file Eio.Path.(dir / backup_path Lockfile.filename))
		);
		test_case "Upgrade detects unknown manifest version" `Quick (fun () ->
			Eio_main.run @@ fun env ->
			let dir = setup_workdir ~env "upgrade-version-detect" in
			write_minimal_manifest ~version:"9.9.9" dir;
			write_minimal_lockfile ~version:"0.1.1" dir;
			Lockfile.lockfile := None;
			let res = upgrade () in
			check bool
				"unknown schema version is rejected"
				true
				(match res with
				| Error (`Upgrade msg) -> String.length msg > 0
				| _ -> false)
		);
	]
