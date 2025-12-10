(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
let working_directory : [`Dir] Eio.Path.t option ref = ref None

let get () =
	match !working_directory with
	| Some dir -> dir
	| None -> failwith "Working directory not set up"

let set ~directory =
	working_directory := Some directory

let set_default ~env () =
	let new_dir =
		let cwd = Eio.Stdenv.cwd env in
		Eio.Path.(cwd / "nix" / "tamal")
	in
	working_directory := Some new_dir

let pp_native_path =
	Fmt.using
		Eio.Path.native
		(Fmt.option (fun ppf -> Fmt.pf ppf " @@ %a" Fmt.string))

(* Without the need for magic strings, we can use tabs in Nix! *)
let root_editor_config_content =
	{|root = true

[*]
charset = utf-8
end_of_line = lf
indent_style = tab
insert_final_newline = true
trim_trailing_whitespace = true
|}

let set_up_editor_config ~dir ~content =
	let editor_config_file = Eio.Path.(dir / ".editorconfig") in
	Logs.app (fun m -> m "Writing new Nixtamal EditorConfig%a …" pp_native_path editor_config_file);
	Eio.Path.with_open_out ~create: (`Or_truncate 0o644) editor_config_file @@ fun flow ->
	Eio.Buf_write.with_flow flow @@ fun writer ->
	Eio.Buf_write.string writer content

let set_up_root () =
	let dir = get () in
	match Eio.Path.kind ~follow: true dir with
	| `Not_found ->
		Logs.app (fun m -> m "Creating Nixtamal directory%a" pp_native_path dir);
		Eio.Path.mkdirs ~perm: 0o755 dir;
		set_up_editor_config ~dir ~content: root_editor_config_content
	| `Directory ->
		Logs.warn (fun m -> m "Nixtamal directory already exists%a" pp_native_path dir)
	| _ ->
		failwith @@ Fmt.str "There is a Nixtamal path, but is not a directory%a" pp_native_path dir

let darcs_context_dir = "darcs_context"

let darcs_context_editor_config_content =
	{|root = true
|}

let set_up_darcs_context_if_needed () : (unit, string) result =
	let dir = Eio.Path.(get () / darcs_context_dir) in
	match Eio.Path.kind ~follow: true dir with
	| `Directory ->
		Ok ()
	| `Not_found ->
		Logs.app (fun m -> m "Creating Nixtamal’s darcs_context directory%a" pp_native_path dir);
		Eio.Path.mkdirs ~perm: 0o755 dir;
		(* as these files are just copied over, unset rules *)
		set_up_editor_config ~dir ~content: darcs_context_editor_config_content;
		Ok ()
	| _ ->
		Error (Fmt.str "There is a Nixtamal path, but is not a directory%a" pp_native_path dir)
