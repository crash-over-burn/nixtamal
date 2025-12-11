(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
open Alcotest
open Nixtamal

let suite =
	[test_case "Manifest latest-cmd to KDL" `Quick (fun () ->
		let kdl = testable Kdl.pp Kdl.equal in
		let open Nixtamal.Input.Latest.Cmd in
		let t = Input.Template.make in
		let in_kdl =
			~${prog = t "curl"; args = [t "https://toast.al"]}
			|: {prog = t "head"; args = [t "-n1"]}
			|> Option.some
			|> Manifest.Latest_cmd.codec.to_kdl
		in
		let out_kdl =
			{|
				latest-cmd {
					$ curl "https://toast.al"
					| head -n1
				}
			|}
			|> Kdl.of_string
			|> Result.get_ok
		in
		check kdl "KDL latest-cmd with pipe" out_kdl in_kdl
	);
	test_case "Manifest latest-cmd of KDL" `Quick (fun () ->
		let latest_cmd = testable Manifest.Latest_cmd.pp Manifest.Latest_cmd.equal in
		let open Nixtamal.Input.Latest.Cmd in
		let t = Input.Template.make in
		let in_latest_cmd =
			let kdl =
				{|
					latest-cmd {
						$ curl "https://toast.al"
						| head -n1
					}
				|}
				|> Kdl.of_string
				|> Result.get_ok
			in
			match Manifest.Latest_cmd.codec.of_kdl kdl with
			| Ok lc -> lc
			| Error err -> failwith Fmt.(str "%a from %a" (list ~sep: semi Util.KDL.Valid.pp_err) err Kdl.pp kdl)
		in
		let out_latest_cmd =
			~${prog = t "curl"; args = [t "https://toast.al"]}
			|: {prog = t "head"; args = [t "-n1"]}
			|> Option.some
		in
		check latest_cmd "latest-cmd with pipe from KDL" out_latest_cmd in_latest_cmd
	);
	test_case "Manifest frozen Pijul to KDL" `Quick (fun () ->
		let kdl = testable Kdl.pp Kdl.equal in
		let t = Input.Template.make in
		let in_kdl =
			let name = Name.Name.make "pijul"
			and kind = `Pijul (Manifest.Pijul.make ~remote: (t "https://nest.pijul.com/pijul/pijul") ~reference: (`Channel "main") ())
			and hash = Manifest.Hash.make ()
			in
			Manifest.Input'.make ~name ~kind ~hash ~latest_cmd: None ~frozen: true ()
			|> Manifest.Input'.codec.to_node
		in
		let out_kdl =
			{|
				pijul frozen=#true {
					pijul {
						remote "https://nest.pijul.com/pijul/pijul"
						channel main
					}
				}
			|}
			|> Kdl.of_string
			|> Result.get_ok
		in
		check kdl "KDL frozen Pijul" out_kdl [in_kdl]
	);
	test_case "Manifest frozen Pijul of KDL" `Quick (fun () ->
		let input = testable Manifest.Input'.pp Manifest.Input'.equal in
		let open Util.KDL.L in
		let open Util.KDL.Valid in
		let t = Input.Template.make in
		let in_input =
			let kdl =
				{|
					pijul frozen=#true {
						pijul {
							remote "https://nest.pijul.com/pijul/pijul"
							channel main
						}
					}
				|}
				|> Kdl.of_string
				|> Result.get_ok
			in
			let node = ll @@ kdl.@(node "pijul" ~nth: 0) in
			match Result.bind node Manifest.Input'.codec.of_node with
			| Ok lc -> lc
			| Error err -> failwith Fmt.(str "%a from %a" (list ~sep: semi Util.KDL.Valid.pp_err) err Kdl.pp kdl)
		in
		let out_input =
			let name = Name.Name.make "pijul"
			and kind = `Pijul (Manifest.Pijul.make ~remote: (t "https://nest.pijul.com/pijul/pijul") ~reference: (`Channel "main") ())
			and hash = Manifest.Hash.make ()
			in
			Manifest.Input'.make ~name ~kind ~hash ~latest_cmd: None ~frozen: true ()
		in
		check input "frozen Pijul from KDL" out_input in_input
	);
	test_case "Manifest frozen Pijul sameshape" `Quick (fun () ->
		let input = testable Manifest.Input'.pp Manifest.Input'.equal in
		let t = Input.Template.make in
		let in_input =
			let name = Name.Name.make "pijul"
			and kind = `Pijul (Manifest.Pijul.make ~remote: (t "https://nest.pijul.com/pijul/pijul") ~reference: (`Channel "main") ())
			and hash = Manifest.Hash.make ()
			in
			Manifest.Input'.make ~name ~kind ~hash ~frozen: true ~latest_cmd: None ()
		in
		let out_input =
			in_input
			|> Manifest.Input'.codec.to_node
			|> Manifest.Input'.codec.of_node
			|> Result.get_ok
		in
		check input "Frozen Pijul KDL roundrip" out_input in_input
	);
	] @
		List.map QCheck_alcotest.to_alcotest [
			QCheck.Test.make
				~name: "Manifest input codec sameshape"
				(QCheck.make ~print: (Fmt.str "%a" Manifest.Input'.pp) Manifest.Input'.gen)
				(fun input ->
					let back_and_forth =
						input
						|> Manifest.Input'.codec.to_node
						|> Manifest.Input'.codec.of_node
					in
					match back_and_forth with
					| Ok input' when input' = input ->
						true
					| Ok input' ->
						QCheck.Test.fail_reportf
							"Aimed for:@,%a@.@.But got:@,%a@."
							Manifest.Input'.pp
							input
							Manifest.Input'.pp
							input'
					| Error err ->
						QCheck.Test.fail_reportf "%a" Fmt.(list ~sep: semi Util.KDL.Valid.pp_err) err;
				);
		]
