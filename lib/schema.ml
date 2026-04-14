(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Version = struct
	type t =
		| V0_1_1
		| V0_2_0
	[@@deriving show, enum, eq, ord]

	let of_string s =
		match s with
		| "0.1.1" -> Some V0_1_1
		| "0.2.0" -> Some V0_2_0
		| _ -> None

	let to_string = function
		| V0_1_1 -> "0.1.1"
		| V0_2_0 -> "0.2.0"

	let current : t = Option.get (of_enum max)

	let versions : t array =
		let vs = Dynarray.create () in
		for idx = min to max do
			match of_enum idx with
			| Some v -> Dynarray.add_last vs v
			| None -> ()
		done;
		Dynarray.to_array vs
end