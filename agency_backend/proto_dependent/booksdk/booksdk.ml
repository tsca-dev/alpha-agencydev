open Book_intf
open Lwt.Infix

module Tm = Tezos_base__TzPervasives.Error_monad

(* let strip_tzresult_lwt ex = *)
(*   match Lwt_main.run ex with *)
(*   | Ok x -> x *)
(*   | Error e -> *)
(*      Format.eprintf "%a" Tm.pp_print_trace e; *)
(*      Stdlib.failwith "tzresult = Error" *)

module OcamlBooksdk = struct
  open TmplversionTypes
  let parse_json : string -> json aresult =
    fun str ->
    try Yojson.Basic.from_string str |> Result.ok
    with Yojson.Json_error msg -> Result.error msg
  let unparse_json : json -> string = Yojson.Basic.to_string

  open Tzutils.Micheline
  open Tzutils

  module Protocol = Tezos_protocol_011_PtHangz2.Protocol
  module MParser = Tezos_client_011_PtHangz2.Michelson_v1_parser
  module MPrinter = Tezos_client_011_PtHangz2.Michelson_v1_printer
  module Prims = Protocol.Michelson_v1_primitives
  type prim = Prims.prim
  type michelson_expr = MParser.parsed
  type michelson_type = MParser.parsed
  type michelson_const = (canonical_location, prim) node

  let parse_expr : string -> michelson_expr =
    fun str -> Michelson.parse_expr str |> strip_tzresult
  let parse_type : string -> michelson_type = parse_expr
  let parse_data : string -> michelson_const =
    fun str ->
    let parsed = Michelson.parse_expr str |> strip_tzresult in
    root (parsed.expanded)

  let unparse_expr : michelson_expr -> string = fun e -> e.source
  let unparse_type : michelson_expr -> string = unparse_expr
  let unparse_data : michelson_const -> string =
    fun c -> Michelson.string_of_expr (strip_locations c)

  let pack_data ?typ data =
    Michelson.pack_data' ?typ (strip_locations data)
    >|= strip_tzresult
  let unpack_data ?typ bytes =
    Michelson.unpack_data ?typ bytes
    >|= strip_tzresult
    >|= root

  let now : unit -> int64 Lwt.t =
    fun () -> Unix.time() |> Int64.of_float |> Lwt.return

  let invalid node fmt =
    Format.kasprintf (fun str ->
        let msg =
          Format.asprintf "%s : %a" str
            MPrinter.print_expr
            (strip_locations node) in
        invalid_arg msg
      ) fmt

  module Michelson_data = struct
    type t = michelson_const

    let to_timestamp : t -> int64 = function
      | Int (_, z) ->
         Z.to_int64 z
      | String (_, str) ->
         Ptime.of_rfc3339 str |> Result.get_ok
         |> fun (x,_,_) -> Ptime.(x |> to_float_s |> Int64.of_float)
      | n -> invalid n "to_timestamp"
    let of_timestamp : int64 -> t =
      fun z -> Int (0, Z.of_int64 z)

    let to_code : t -> michelson_expr = function
      | Seq _ as code ->
         unparse_data code |> parse_expr
      | n -> invalid n "to_code"
    let of_code : michelson_expr -> t = fun e ->
      match root (e.expanded) with
      | Seq _ as code -> code
      | n -> invalid n "of_code"

    let to_address = function
      | String (_, str) -> str
      | Bytes (_, bytes) ->
         Protocol.Contract_repr.(
          Data_encoding.Binary.of_bytes encoding
            bytes |> Result.get_ok
          |> to_b58check)
      | n -> invalid n "to_address"
    let of_address str = String (0, str)

    let to_key_hash = function
      | String (_, str) -> str
      | Bytes (_, bytes) ->
         Protocol.Environment.Signature.Public_key_hash.(
          Data_encoding.Binary.of_bytes encoding
            bytes |> Result.get_ok
          |> to_b58check)
      | n -> invalid n "to_key_hash"
    let of_key_hash str = String (0, str)

    let to_string = function
      | String (_, str) -> str
      | n -> invalid n "to_string"
    let of_string str =
      let str = Protocol.Environment.Signature.Public_key_hash.(
          of_b58check_exn str |> to_b58check) in
      String (0, str)

    let to_mutez = function
      | Int (_, z) -> Z.to_int64 z
      | n -> invalid n "to_mutez"
    let of_mutez i = Int (0, Z.of_int64 i)

    open Prims

    let to_bool = function
      | Prim (_, D_True, _, _) -> true
      | Prim (_, D_False, _, _) -> false
      | n -> invalid n "to_bool"
    let of_bool b =
      let p = if b then D_True else D_False in
      Prim (0, p, [], [])

    let to_unit = function
      | Prim (_, D_Unit, _, _) -> ()
      | n -> invalid n "to_unit"
    let of_unit() = Prim (0, D_Unit, [], [])

    let to_int = function
      | Int (_, z) ->
         (try Some (Z.to_int32 z) with Z.Overflow -> None)
      | n  -> invalid n "to_int"
    let of_int i = Int (0, Z.of_int32 i)
    let to_bigint = function
      | Int (_, z) -> (Z.to_string z)
      | n  -> invalid n "to_bigint"
    let of_bigint str = Int (0, Z.of_string str)

    let to_bytes = function
      | String (_, str) ->
         Commons.bytes_of_prefixed_hex str
      | Bytes (_, b) -> b
      | n -> invalid n "to_bytes"
    let of_bytes b = Bytes (0, b)

    let to_option = function
      | Prim (_, D_None, _, _) -> None
      | Prim (_, D_Some, [t], _) -> Some t
      | n -> invalid n "to_option"
    let of_option = function
      | None -> Prim (0, D_None, [], [])
      | Some t -> Prim (0, D_Some, [t], [])

    let to_list = function
      | Seq (_, ts) -> ts
      | n -> invalid n "to_list"
    let of_list ts = Seq (0, ts)

    let to_set = to_list
    let of_set = of_list

    let to_map = function
      | Seq (_, ts) ->
         let elt = function
           | Prim (_, D_Elt, [k; v], _) -> k,v
           | n -> invalid n "to_map" in
         ts |&> elt
      | n -> invalid n "to_list"
    let of_map ts =
      let elt (k,v) = Prim (0, D_Elt, [k; v], []) in
      Seq (0, ts |&> elt)

    let to_pair = function
      | Prim (_, D_Pair, [l; r], _) -> l, r
      | n -> invalid n "to_pair"
    let of_pair (l,r) = Prim (0, D_Pair, [l; r], [])

    let to_left = function
      | Prim (_, D_Left, [t], _) -> t
      | n -> invalid n "to_left"
    let to_right = function
      | Prim (_, D_Right, [t], _) -> t
      | n -> invalid n "to_left"
    let to_or = function
      | Prim (_, D_Left, [t], _) -> `Left t
      | Prim (_, D_Right, [t], _) -> `Right t
      | n -> invalid n "to_or"
    let of_or = function
      | `Left t -> Prim (0, D_Left, [t], [])
      | `Right t -> Prim (0, D_Right, [t], [])
  end
end

let ocaml_booksdk :TmplversionTypes.booksdk  = (module OcamlBooksdk)
