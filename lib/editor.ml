(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
let find () =
	match Sys.getenv_opt "VISUAL" with
	| Some v -> v
	| None ->
		match Sys.getenv_opt "EDITOR" with
		| Some e -> e
		| None -> "vi"

let run_on file =
	match find () with
	| ed when String.contains ed ' ' ->
		Unix.execvp "/bin/sh" [|"/bin/sh"; "-c"; ed ^ " " ^ file|]
	| ed ->
		Unix.execvp ed [|ed; file|]
