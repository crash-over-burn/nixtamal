(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Formatter = struct
	let to_flow pp flow =
		let buffer = Buffer.create 4096 in
		let fmt = Format.formatter_of_buffer buffer in
		pp fmt;
		Format.pp_print_flush fmt ();
		Eio.Flow.copy_string (Buffer.contents buffer) flow
end

module Jsont = struct
	include Jsont

	let encode_tag tag_code encoder v = [|
		(Json.encode uint8) tag_code |> Result.get_ok;
		Json.encode encoder v |> Result.get_ok;
	|]

	let pp_piset fmt json =
		let rec pp_value fmt = function
			| Jsont.Null _ -> Fmt.pf fmt "%a" Jsont.pp_null ()
			| Jsont.Bool (b, _) -> Fmt.pf fmt "%a" Jsont.pp_bool b
			| Jsont.Number (f, _) -> Fmt.pf fmt "%a" Jsont.pp_number f
			| Jsont.String (s, _) -> Fmt.pf fmt "%a" Jsont.pp_string s
			| Jsont.Array (arr, _) ->
				Fmt.pf fmt "@[<hov>[%a]@]" (Fmt.list ~sep: (Fmt.any ",@ ") pp_value) arr
			| Jsont.Object (obj, _) ->
				let pp_mem fmt ((k, _), v) =
					Fmt.pf fmt "@[<hv>%a: %a@]" Jsont.pp_string k pp_value v
				in
				Fmt.pf fmt "@[<hv>{@;<0 1>%a@;<0 0>}@]" (Fmt.list ~sep: (Fmt.any ",@ ") pp_mem) obj
		in
		pp_value fmt json

	let of_flow codec flow =
		try
			let buf = Eio.Buf_read.of_flow flow ~max_size: max_int in
			let str = Eio.Buf_read.take_all buf in
			Jsont_bytesrw.decode_string codec str
		with
			| exn -> Error (Printexc.to_string exn)

	let to_flow codec value flow =
		match Jsont_bytesrw.encode_string codec value with
		| Ok str ->
			Eio.Flow.copy_string str flow;
			Ok ()
		| Error err -> Error err

	let to_flow_piset codec value flow =
		match Jsont.Json.encode codec value with
		| Ok json ->
			let buffer = Buffer.create 4096 in
			let fmt = Format.formatter_of_buffer buffer in
			let base_fmt = Format.pp_get_formatter_out_functions fmt () in
			let tabbed_fmt = {base_fmt with
				out_indent = (fun n -> base_fmt.out_string (String.make n '\t') 0 n)
			}
			in
			Format.pp_set_formatter_out_functions fmt tabbed_fmt;
			pp_piset fmt json;
			Format.pp_print_flush fmt ();
			Eio.Flow.copy_string (Buffer.contents buffer) flow;
			Ok ()
		| Error err -> Error err
end

module KDL = struct
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
		Kdl.pp ppf doc;
		Format.pp_print_flush ppf ()

	module L = Kdl_lens_result

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
		to_kdl: 'a -> Kdl.t;
		of_kdl: Kdl.t -> 'a Valid.t
	}

	type 'a node_codec = {
		to_node: 'a -> Kdl.node;
		of_node: Kdl.node -> 'a Valid.t
	}
end

module These = struct
	type ('a, 'b) t =
		| This of 'a
		| That of 'b
		| These of 'a * 'b
	[@@deriving show]

	let map_both f g = function
		| This a -> This (f a)
		| That b -> That (g b)
		| These (a, b) -> These (f a, g b)

	let map_this f =
		map_both f Fun.id

	let map_that g =
		map_both Fun.id g

	let these f g h = function
		| This a -> f a
		| That b -> g b
		| These (a, b) -> h a b

	let merge h =
		these Fun.id Fun.id h

	let merge_with h f g =
		these f g (fun a b -> h (f a) (g b))
end

module URI = struct
	let jsont : Uri.t Jsont.t =
		Jsont.string
		|> Jsont.map ~kind: "URI" ~dec: Uri.of_string ~enc: Uri.to_string
end

module Non_empty_list = struct
	type 'a t =
			('a * 'a list)
	[@@deriving show, eq, qcheck]

	let to_list (x, xs) = x :: xs

	let of_list = function
		| [] -> None
		| x :: xs -> Some (x, xs)

	let map f (x, xs) = (f x, List.map f xs)
	let fold_left f acc (x, xs) = List.fold_left f acc (x :: xs)
	let fold_right f acc (x, xs) = List.fold_right f acc (x :: xs)
end
