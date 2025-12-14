(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
let prefixed_env_info ?doc ?deprecated var =
	Cmdliner.Cmd.Env.info ("NIXTAMAL_" ^ var) ?doc ?deprecated

let common_man = [
	`S "BUGS";
	`P "During alpha, contact the maker directly or join the XMPP MUC.";
	`S "SEE ALSO";
	`I ("nixtamal-manifest(5)", "manifest.kdl format (TODO: & schema)");
]

module Global = struct
	type t = {
		style_renderer: Fmt.style_renderer option;
		level: Logs.level option;
		dir: string option;
		jobs: int;
	}

	let directory_arg =
		let open Cmdliner in
		let env = prefixed_env_info ~doc: "Directory for Nixtamal" "DIRECTORY" in
		Arg.(
			value
			& opt (some string) None
			& info
				["directory"]
				~env
				~doc: "Working directory for Nixtamal-related files (default: $PWD/nix/tamal)"
				~docv: "PATH"
		)

	let jobs_arg =
		let open Cmdliner in
		let domain_count : int = Stdlib.Domain.recommended_domain_count () in
		Arg.(
			value
			& opt int domain_count
			& info
				["j"; "jobs"]
				~env: (prefixed_env_info "JOBS")
				~doc: "Nixtamal’s executor pool domain size."
				~docv: "INT"
		)

	let args =
		let open Cmdliner in
		let open Term in
		ret
			(
				const (fun style_renderer level dir jobs ->
					`Ok {style_renderer; level; dir; jobs}
				)
				$ Fmt_cli.style_renderer ~env: (prefixed_env_info "OUTPUT_COLOR") ()
				$ Logs_cli.level ~env: (prefixed_env_info "LOG_LEVEL") ()
				$ directory_arg
				$ jobs_arg
			)

	let run ~env {style_renderer; level; dir; jobs} =
		Fmt_tty.setup_std_outputs ?style_renderer ();
		Logs.set_level (
			match level with
			| None -> Some Logs.Info
			| Some lvl -> Some lvl
		);
		Logs.set_reporter (Logs_fmt.reporter ());
		Kdl.indent := 1;
		let () =
			match dir with
			| None ->
				Nixtamal.Working_directory.set_default ~env ()
			| Some d ->
				let cwd = Eio.Stdenv.cwd env in
				let directory = Eio.Path.(cwd / d) in
				Nixtamal.Working_directory.set ~directory
		in
		fun f -> f ~env ~domain_count: jobs
end

module Set_up = struct
	let nixpkgs_mismatch = "Both --nixpkgs-branch & --nixpkgs-ref cannot be used at the same time"

	let info =
		Cmdliner.Cmd.info
			"set-up"
			~doc: "Set up working directory for Nixtamal. By default, also adds Nixpkgs from upstream to the project’s inputs."
			~man: common_man

	let run ~env ~domain_count: _ nixpkgs : unit =
		match Nixtamal.set_up ~env ?nixpkgs () with
		| Ok() -> ()
		| Error err -> failwith (Fmt.str "%a" Nixtamal.Error.pp_error err)

	let term ~env =
		let open Cmdliner in
		let no_nixpkgs_arg =
			Arg.(
				value
				& flag
				& info
					["no-nixpkgs"]
					~env: (prefixed_env_info "NO_NIXPKGS")
					~doc: "Do not add Nixpkgs to the pinned inputs list by default."
			)
		and use_nixpkgs_git_mirrors_arg =
			Arg.(
				value
				& flag
				& info
					["use-nixpkgs-git-mirrors"]
					~env: (prefixed_env_info "USE_NIXPKGS_GIT_MIRRORS")
					~doc: "For resiliance, add known Nixpkgs git mirrors to fallback on when the Nixpkgs’s Microsoft GitHub host inevitably goes down again. Off by default as the Git updating is slightly slower & some users might object to TUNA’s hosting origin state."
			)
		and nixpkgs_branch_arg =
			Arg.(
				value
				& opt (some string) None
				& info
					["nixpkgs-branch"]
					~env: (prefixed_env_info "NIXPKGS_BRANCH")
					~doc: (Fmt.str "Nixpkgs Git branch for Nixtamal setup (shorthand for refs/branches/*). %s." nixpkgs_mismatch)
					~docv: "BRANCH_NAME"
			)
		and nixpkgs_ref_arg =
			Arg.(
				value
				& opt (some string) None
				& info
					["nixpkgs-ref"]
					~env: (prefixed_env_info "NIXPKGS_REF")
					~doc: (Fmt.str "Nixpkgs Git ref for Nixtamal setup (default: %s). %s." Nixtamal.Input.Nixpkgs.default_ref nixpkgs_mismatch)
					~docv: "REF"
			)
		in
		let nixpkgs_reference_arg =
			let open Term in
			let mk_reference nixpkgs_branch nixpkgs_ref =
				match nixpkgs_branch, nixpkgs_ref with
				| None, None -> `Ok None
				| Some branch, None -> `Ok (Some (`Branch branch))
				| None, Some ref -> `Ok (Some (`Ref ref))
				| Some _, Some _ -> `Error (true, nixpkgs_mismatch)
			in
			ret
				(
					const mk_reference
					$ nixpkgs_branch_arg
					$ nixpkgs_ref_arg
				)
		and nixpkgs_revision_arg =
			Arg.(
				value
				& opt (some string) None
				& info
					["nixpkgs-revision"]
					~env: (prefixed_env_info "NIXPKGS_REVISION")
					~doc: ("Nixpkgs Git revision for Nixtamal setup. The value will be used as the latest revision/change.")
					~docv: "REVISION"
			)
		in
		let nixpkgs_arg =
			let open Term in
			let open Nixtamal.Input in
			let mk_arg no_nixpkgs use_nixpkgs_git_mirrors (reference : Git.Reference.t option) nixpkgs_revision =
				if no_nixpkgs then
					`Ok None
				else if use_nixpkgs_git_mirrors then
					let latest_revision = nixpkgs_revision in
					let input = Nixpkgs.make_git_with_known_mirrors ?reference ?latest_revision () in
					`Ok (Some input)
				else
					let latest_value = nixpkgs_revision in
					let input = Nixpkgs.make_archive ?reference ?latest_value () in
					`Ok (Some input)
			in
			ret
				(
					const mk_arg
					$ no_nixpkgs_arg
					$ use_nixpkgs_git_mirrors_arg
					$ nixpkgs_reference_arg
					$ nixpkgs_revision_arg
				)
		in
		Term.(
			const (fun glb -> Global.run ~env glb @@ run)
			$ Global.args
			$ nixpkgs_arg
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module Check_soundness = struct
	let info =
		Cmdliner.Cmd.info
			"check-soundness"
			~doc: "Checks that the manifest × lockfile is sound."
			~man: common_man

	let run ~env ~domain_count: _ : unit =
		match Nixtamal.check_soundness ~env () with
		| Ok() -> ()
		(* TODO: use these errors for error codes *)
		| Error err -> failwith (Fmt.str "%a" Nixtamal.Error.pp_error err)

	let term ~env =
		let open Cmdliner in
		Term.(
			const (fun glb -> Global.run ~env glb @@ run)
			$ Global.args
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module Tweak = struct
	let info =
		Cmdliner.Cmd.info
			"tweak"
			~doc: "Tweak the manifest file with \\$VISUAL, \\$EDITOR, or vi"
			~man: common_man

	let run ~env ~domain_count: _ : unit =
		match Nixtamal.tweak ~env () with
		| Ok() -> ()
		(* TODO: use these errors for error codes *)
		| Error err -> failwith (Fmt.str "%a" Nixtamal.Error.pp_error err)

	let term ~env =
		let open Cmdliner in
		Term.(
			const (fun glb -> Global.run ~env glb @@ run)
			$ Global.args
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module Show = struct
	let info =
		Cmdliner.Cmd.info
			"show"
			~doc: "Shows current inputs as understood by Nixtamal for earthlings."
			~man: common_man

	let run ~env ~domain_count: _ : unit =
		match Nixtamal.show ~env () with
		| Ok() -> ()
		| Error err -> failwith (Fmt.str "%a" Nixtamal.Error.pp_error err)

	let term ~env =
		let open Cmdliner in
		Term.(
			const (fun glb -> Global.run ~env glb @@ run)
			$ Global.args
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module Lock = struct
	let info =
		Cmdliner.Cmd.info
			"lock"
			~doc: "Lock all not-yet-locked inputs."
			~man: common_man

	let run ~env ~domain_count force names : unit =
		let names = List.map Nixtamal.Name.Name.make names in
		match Nixtamal.lock ~env ~domain_count ~force ~names () with
		| Ok() -> ()
		| Error err -> failwith (Fmt.str "%a" Nixtamal.Error.pp_error err)

	let term ~env =
		let open Cmdliner in
		let force_arg =
			Arg.(
				value
				& flag
				& info ["f"; "force"] ~doc: "Force input to lock (useful if changing the manifest in a manner that otherwise wouldn’t trigger a lock)."
			)
		and names_arg =
			Arg.(
				value
				& pos_all string []
				& info [] ~docv: "INPUT_NAME" ~doc: "Input names to lock (if already locked, will skip)."
			)
		in
		Term.(
			const (fun glb force -> Global.run ~env glb @@ run force)
			$ Global.args
			$ force_arg
			$ names_arg
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module List_stale = struct
	let info =
		Cmdliner.Cmd.info
			"list-stale"
			~doc: "List stale inputs with latest-cmd, without refreshing"
			~man: common_man

	let run ~env ~domain_count : unit =
		match Nixtamal.list_stale ~env ~domain_count with
		| Ok() -> ()
		| Error err -> failwith (Fmt.str "%a" Nixtamal.Error.pp_error err)

	let term ~env =
		let open Cmdliner in
		Term.(
			const (fun glb -> Global.run ~env glb @@ run)
			$ Global.args
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module Refresh = struct
	let info =
		Cmdliner.Cmd.info
			"refresh"
			~doc: "Refreshes all non-frozen inputs using the latest-cmd — or the default latest-cmd for certain kinds with a reasonable default (Git)."
			~man: common_man

	let run ~env ~domain_count names : unit =
		let names = List.map Nixtamal.Name.Name.make names in
		match Nixtamal.refresh ~env ~domain_count ~names () with
		| Ok() -> ()
		| Error err -> failwith (Fmt.str "%a" Nixtamal.Error.pp_error err)

	let term ~env =
		let open Cmdliner in
		let names_arg =
			Arg.(
				value
				& pos_all string []
				& info [] ~docv: "INPUT_NAME" ~doc: "Input names to refresh."
			)
		in
		Term.(
			const (fun glb -> Global.run ~env glb @@ run)
			$ Global.args
			$ names_arg
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end
