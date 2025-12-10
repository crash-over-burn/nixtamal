(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Name = struct
	type t =
		Name of string
	[@@unboxed]
	[@@deriving eq]

	let [@inline]make n = Name n
	let [@inline]take (Name n) = n

	let pp fmt name =
		(* it’s okay to have fun *)
		Fmt.pf fmt "「%s」" (take name)

	(* String.compare but with nixpkgs at the top *)
	let compare (Name a) (Name b) =
		let prio x =
			if x = "nixpkgs" then
				0
			else if String.starts_with ~prefix: "nixpkgs" x then
				1
			else
				2
		in
		match Stdlib.compare (prio a) (prio b) with
		| 0 -> String.compare a b
		| d -> d
end

module NameHashtbl : sig
		type key = Name.t
		include Hashtbl.S with type key := Name.t
	end
= Hashtbl.Make(struct
	type t = Name.t
	let equal = Name.equal
	let hash n = Hashtbl.hash (Name.take n)
end)

module NameMap = struct
	module Impl = Map.Make(struct
		type t = Name.t
		let compare = Name.compare
	end)

	include Impl

	type 'a t = 'a Impl.t

	let pp pp_value fmt map =
		Fmt.list
			~sep: (Fmt.any "; ")
			(fun ppf (key, value) -> Fmt.pf ppf "%s ↦ %a" (Name.take key) pp_value value)
			fmt
			(bindings map)

	let jsont ?kind ?doc (type' : 'a Jsont.t) : 'a t Jsont.t =
		let name_map =
			let dec_empty () = empty
			and dec_add _meta key value mems = add (Name.make key) value mems
			and dec_finish _meta mems = mems
			and enc f mems acc =
				fold (fun n v acc -> f Jsont.Meta.none (Name.take n) v acc) mems acc
			in
			Jsont.Object.Mems.map type' ~dec_empty ~dec_add ~dec_finish ~enc: {enc}
		in
		Jsont.Object.map ?kind ?doc Fun.id
		|> Jsont.Object.keep_unknown name_map ~enc: Fun.id
		|> Jsont.Object.finish
end
