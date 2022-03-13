open Tezos_base__TzPervasives
open Tezos_shell_services

module Micheline = Tezos_micheline.Micheline
module Protocol = Tezos_protocol_006_PsCARTHA.Protocol

type Error_monad.error += Encoding_error of Data_encoding.Binary.write_error
type Error_monad.error +=
   | Decoding_error of Data_encoding.Binary.read_error
   | Decoding_error_str of string

let michelson_expr_encoding =
  Tezos_micheline.Micheline.canonical_encoding_v1
    ~variant:"michelson_v1"
    Protocol.Michelson_v1_primitives.prim_encoding

let packed_data_prefix = Bytes.of_string "\005"

let fake_alpha_context =
  let open Tezos_006_PsCARTHA_test_helpers in
  let module Protocol = Tezos_protocol_006_PsCARTHA.Protocol in
  Context.init 1
  >>=? fun (block, _accounts) ->
  let Block.{
      context;
      header =
        { shell =
            {level; timestamp = predecessor_timestamp;
             fitness; _};
          _ }
      ; _ } = block in
  let now = Unix.time () |> Int64.of_float in
  let timestamp = Time.Protocol.of_seconds now in
  (Protocol.Alpha_context.prepare
     ~level ~predecessor_timestamp ~timestamp ~fitness
     context)
  >|= Protocol.Environment.wrap_error

let pack_data_untyped data =
  Lwt.return &
  Tezos_client_006_PsCARTHA.Michelson_v1_parser.(data.unexpanded)

  |> Protocol.Michelson_v1_primitives.prims_of_strings
  |> Protocol.Environment.wrap_error
  >>? fun converted ->
  Data_encoding.Binary.to_bytes michelson_expr_encoding converted
  |> begin function
       | Error e -> error (Encoding_error e)
       | Ok bytes -> ok bytes
     end
  >>? fun bytes ->
  Bytes.cat packed_data_prefix bytes |> ok

let pack_data_typed ~ctxt data typ =
  Lwt.map Protocol.Environment.wrap_error &
  let open Protocol.Script_ir_translator in
  let open Protocol.Environment.Error_monad in
  let legacy = true in
  let tr data = Tezos_client_006_PsCARTHA.Michelson_v1_parser.(data.unexpanded) in
  let (typ, node) = tr typ, tr data in
  Protocol.Michelson_v1_primitives.prims_of_strings node |> Lwt.return
  >>=? fun node ->
  Protocol.Michelson_v1_primitives.prims_of_strings typ |> Lwt.return
  >>=? fun typ ->
  let (typ, node) = (Micheline.root typ, Micheline.root node) in
  parse_packable_ty ctxt ~legacy typ |> Lwt.return
  >>=? fun (Ex_ty typ, ctxt) ->
  parse_data ctxt ~legacy typ node
  >>=? fun (data, ctxt) ->
  Protocol.Script_ir_translator.pack_data ctxt typ data
  >>=? fun (bytes, _ctxt) -> return bytes

let pack_data ?typ data =
  match typ with
  | None -> pack_data_untyped data
  | Some typ ->
     fake_alpha_context >>=? fun ctxt ->
     pack_data_typed ~ctxt data typ

let deprefix prefix bytes =
  let len = Bytes.length bytes in
  let plen = Bytes.length prefix in
  let err = Decoding_error_str "prefix check failed" in
  fail_when (len < plen) err
  >>=? fun () ->
  try prefix
      |> Bytes.iteri (fun i c ->
             if Bytes.get bytes i <> c then raise Not_found);
      return (Bytes.sub bytes plen (len-plen))
  with _ -> fail err

let unpack_data_untyped data =
  deprefix packed_data_prefix data
  >>=? fun data ->
  Data_encoding.Binary.of_bytes michelson_expr_encoding data
  |> begin function
       | Error e -> fail (Decoding_error e)
       | Ok decoded -> return decoded
     end

let unpack_data_typed ~ctxt data typ =
  unpack_data_untyped data
  >>=? fun node -> begin
      let open Protocol.Script_ir_translator in
      let open Protocol.Environment.Error_monad in
      let legacy = true in
      Tezos_client_006_PsCARTHA.Michelson_v1_parser.(typ.unexpanded)
      |>  Protocol.Michelson_v1_primitives.prims_of_strings |> Lwt.return
      >>=? fun typ ->
      parse_packable_ty ctxt ~legacy (Micheline.root typ) |> Lwt.return
      >>=? fun (Ex_ty typ, ctxt) ->
      parse_data ctxt ~legacy typ
        (Micheline.root node)
      >>=? fun (x, ctxt) ->
      unparse_data ctxt Readable typ x
       >>=? fun (node, _ctxt) ->
       Micheline.strip_locations node |> return
    end
  >|= Protocol.Environment.wrap_error

let unpack_data ?typ (data : bytes) =
  match typ with
  | None -> unpack_data_untyped data
  | Some typ ->
     fake_alpha_context >>=? fun ctxt ->
     unpack_data_typed ~ctxt data typ

let parse_expr expr =
  let open Tezos_micheline in
  let open Tezos_client_006_PsCARTHA in
  Lwt.return @@ Micheline_parser.no_parsing_error
  @@ Michelson_v1_parser.parse_expression expr

let strip_tzresult_lwt ex =
  match Lwt_main.run ex with
  | Ok x -> x
  | Error e ->
     Format.eprintf "%a" pp_print_error e;
     Stdlib.failwith "tzresult = Error"

let strip_tzresult : 'a tzresult -> 'a = function
  | Ok x -> x
  | Error e ->
     Format.eprintf "%a" pp_print_error e;
     Stdlib.failwith "tzresult = Error"

module Handy = struct

  let _ = Lwt_main.run

  let strip_tzresult_lwt ?failwith_reason:(msg="error") ex =
    match Lwt_main.run ex with
    | Ok x -> x
    | Error e ->
       Format.eprintf "%a" pp_print_error e;
       Stdlib.failwith msg

  let pack_data ?typ data =
    let progn =
      (match typ with
       | None -> return None
       | Some typ -> parse_expr typ >>|? Option.some)
      >>=? fun typ ->
      parse_expr data >>=? fun data ->
      pack_data ?typ data in
    strip_tzresult_lwt
      ~failwith_reason:"pack_data"
      progn
end

let hex_of_bytes_prefixed bytes =
  Format.asprintf "0x%a"
    Hex.pp (Hex.of_bytes bytes)

let hex_of_bytes bytes =
  Format.asprintf "%a"
    Hex.pp (Hex.of_bytes bytes)

let bytes_of_hex str =
  Hex.to_bytes (`Hex str)

let bytes_of_hex_prefixed s =
  Hex.to_bytes (`Hex (String.sub s 2 (String.length s - 2)))

let chain_of_string str = Chain_services.parse_chain str |> Result.get_ok

let block_of_string str = Block_services.parse_block str |> Result.get_ok


