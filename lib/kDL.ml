(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(* extend & fix casing for ocaml-kdl *)
include Kdl

let of_flow flow =
	try
		Eio.Buf_read.parse_exn
			(fun buf -> Eio.Buf_read.take_all buf |> Kdl.of_string)
			~max_size: max_int
			flow
	with
		| exn -> failwith (Printexc.to_string exn)

let to_flow flow doc =
	Eio.Buf_write.with_flow flow @@ fun buf ->
	let out_string s off len =
		String.sub s off len |> Eio.Buf_write.string buf
	in
	let flush () = () in
	let ppf = Format.make_formatter out_string flush in

	(* replace spaces with tabs for indentation *)
	let base_fmt = Format.pp_get_formatter_out_functions ppf () in
	let tabbed_fmt = {base_fmt with
		out_indent = (fun n -> base_fmt.out_string (String.make n '\t') 0 n)
	}
	in
	Format.pp_set_formatter_out_functions ppf tabbed_fmt;

	(* enable utf-8 and pretty-print *)
	Fmt.set_utf_8 ppf true;
	pp ppf doc;
	Format.pp_print_flush ppf ()

module L = KDL_lens_result

module Valid = struct
	type err = [
		| L.lerr
		| `ParseError of Kdl.error
		| `OneRequired of string list
		| `OnlyOneOf of string list
		| `InvalidLatestCmd of string
		| `InvalidHashAlgorithm of string
	]
	[@@deriving show]
	type 'a t = ('a, err list) result
	let pp ~(ok : 'a Fmt.t) : 'a t Fmt.t =
		Fmt.result ~ok ~error: (Fmt.list pp_err)
	let map = Result.map
	let map1 (f : 'a -> 'b) (vx : ('a, err) result) : 'b t =
		match vx with
		| Ok x -> Ok (f x)
		| Error e -> Error [e]
	let map_error = Result.map_error
	let pure x = Ok x
	let and_map (vx : 'a t) (vf : ('a -> 'b) t) =
		match vx, vf with
		| Ok x, Ok f -> Ok (f x)
		| Error e, Ok _ -> Error e
		| Ok _, Error e -> Error e
		| Error e1, Error e2 -> Error (e2 @ e1)
	let product (vx : 'a t) (vy : 'b t) =
		match vx, vy with
		| Ok x, Ok y -> Ok (x, y)
		| Error e, Ok _ -> Error e
		| Ok _, Error e -> Error e
		| Error e1, Error e2 -> Error (e2 @ e1)
	let and_map1 (vx : ('a, err) result) (vf : ('a -> 'b) t) =
		match vx, vf with
		| Ok x, Ok f -> Ok (f x)
		| Error e, Ok _ -> Error [e]
		| Ok _, Error e -> Error e
		| Error e1, Error e2 -> Error (e2 @ [e1])
	let bind = Result.bind
	let lift_lens (r : ('a, L.lerr) result) : ('a, err list) result =
		Result.map_error (fun e -> ([e] :> err list)) r
	let ll = lift_lens
	let (let+) vx f = map f vx
	let (and+) = product
	let (let*) = bind
end

type 'a codec = {
	to_kdl: 'a -> t;
	of_kdl: t -> 'a Valid.t
}

type 'a node_codec = {
	to_node: 'a -> node;
	of_node: node -> 'a Valid.t
}
