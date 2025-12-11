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
