(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
let info =
	let top_level_man = [
		`S "LICENSE";
		`P "GNU General Public License, version 3.0 later (GPL-3.0-or-later)";
		`S "MAKER";
		`P "toastal <https://toast.al/contact/>";
		`S "FUNDING";
		`P "See: https://toast.al/funding/";
	]
	in
	Cmdliner.Cmd.info
		"nixtamal"
		~version: "@version@"
		~doc: "fulfilling input pinning for Nix"
		~man: (top_level_man @ Cmd.common_man)

let cmd ~env =
	let subcommands = [
		Cmd.Set_up.cmd ~env;
		Cmd.Check_soundness.cmd ~env;
		Cmd.Tweak.cmd ~env;
		Cmd.Show.cmd ~env;
		Cmd.Lock.cmd ~env;
		Cmd.Refresh.cmd ~env;
	]
	in
	Cmdliner.Cmd.group info subcommands

let () =
	Eio_main.run @@ fun env ->
	(* if !Sys.interactive then () else *)
	exit @@ Cmdliner.Cmd.eval (cmd ~env)
