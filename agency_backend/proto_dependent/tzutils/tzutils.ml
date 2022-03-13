open Tezos_base__TzPervasives

module Micheline = Tezos_micheline.Micheline
module Protocol = Tezos_protocol_011_PtHangz2.Protocol
module Env = Protocol.Environment
module Em = Env.Error_monad

module Tm = Tezos_base__TzPervasives.Error_monad
module Michelson_v1_parser = Tezos_client_011_PtHangz2.Michelson_v1_parser

type Error_monad.error += Encoding_error of Data_encoding.Binary.write_error
type Error_monad.error +=
   | Decoding_error of Data_encoding.Binary.read_error
   | Decoding_error_str of string

include Commons.Hexbytes

module Private = struct
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
end open Private

(* dealing with tzresult and encoding *)

let strip_lwt ex = Lwt_main.run ex

let strip_tzresult : 'a tzresult -> 'a = function
  | Ok x -> x
  | Error e ->
     Format.eprintf "%a" Tm.pp_print_trace e;
     Stdlib.failwith "tzresult = Error"

(* dealing with michelson *)

let fake_alpha_context =
  let open Tezos_011_PtHangz2_test_helpers in
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
  >|= Env.wrap_tzresult

module Michelson = struct

  let parse_expr expr =
    let open Tezos_micheline in
    let open Tezos_client_011_PtHangz2 in
    Micheline_parser.no_parsing_error
    @@ Michelson_v1_parser.parse_expression expr

  let string_of_expr : Protocol.Alpha_context.Script.expr -> string =
    fun expr ->
    let open Tezos_client_011_PtHangz2 in
    let open Protocol.Michelson_v1_primitives in
    let expanded =
      expr |> strings_of_prims |> Micheline.root
      |> Michelson_v1_macros.expand_rec |> fst (* |> strip_tzresult *)
      |> Micheline.strip_locations
      |> prims_of_strings |> Env.wrap_tzresult |> strip_tzresult in
    Format.asprintf "%a"
      Michelson_v1_printer.print_expr
      (* expr *) expanded

  let script_expr_of_parsed expr =
    Tezos_client_011_PtHangz2.Michelson_v1_parser.(expr.unexpanded)
    |> Protocol.Michelson_v1_primitives.prims_of_strings
    |> Env.wrap_tzresult

  let hash_parsed_expr  ~ctxt expr typ =
    let open Protocol.Script_ir_translator in
    let legacy = true in
    script_expr_of_parsed typ |> Lwt.return >>=? fun typ ->
    script_expr_of_parsed expr |> Lwt.return >>=? fun expr ->
    ( let open Protocol.Environment.Error_monad in
      let (typ, expr) = Micheline.(root typ, root expr) in
      parse_packable_ty ctxt ~legacy typ |> Lwt.return
      >>=? fun (Ex_ty typ, ctxt) ->
      parse_data ~allow_forged:true ctxt ~legacy typ expr
      >>=? fun (expr, ctxt) ->
      Protocol.Script_ir_translator.hash_data ctxt typ expr
      >>=? fun (hash, _) -> return hash)
    >|= Env.wrap_tzresult

  let michelson_expr_encoding =
    Env.Micheline.canonical_encoding_v1
      ~variant:"michelson_v1"
      Protocol.Michelson_v1_primitives.prim_encoding

  let packed_data_prefix = Bytes.of_string "\005"

  let pack_data_untyped data =
    Lwt.return (
        Tezos_client_011_PtHangz2.Michelson_v1_parser.(data.unexpanded)
        |> Protocol.Michelson_v1_primitives.prims_of_strings
        |> Env.wrap_tzresult
        >>? fun converted ->
            Data_encoding.Binary.to_bytes michelson_expr_encoding converted
            |> begin function
                 | Error e -> error (Encoding_error e)
                 | Ok bytes -> ok bytes
               end
            >>? fun bytes ->
                Bytes.cat packed_data_prefix bytes |> ok)

  let pack_data_typed ~ctxt data typ =
    Lwt.map Env.wrap_tzresult (
        let open Protocol.Script_ir_translator in
        let open Protocol.Environment.Error_monad in
        let legacy = true in
        let tr data = Michelson_v1_parser.(data.unexpanded) in
        let (typ, node) = tr typ, tr data in
        Protocol.Michelson_v1_primitives.prims_of_strings node |> Lwt.return
        >>=? fun node ->
        Protocol.Michelson_v1_primitives.prims_of_strings typ |> Lwt.return
        >>=? fun typ ->
        let (typ, node) = (Micheline.root typ, Micheline.root node) in
        parse_packable_ty ctxt ~legacy typ |> Lwt.return
        >>=? fun (Ex_ty typ, ctxt) ->
        parse_data ctxt ~allow_forged:true ~legacy typ node
        >>=? fun (data, ctxt) ->
        Protocol.Script_ir_translator.pack_data ctxt typ data
        >>=? fun (bytes, _ctxt) -> return bytes)

  let pack_data ?typ data =
    match typ with
    | None -> pack_data_untyped data
    | Some typ ->
       fake_alpha_context >>=? fun (ctxt,_,_) ->
       pack_data_typed ~ctxt data typ

  let pack_data_untyped' data =
    Lwt.return (
        Data_encoding.Binary.to_bytes michelson_expr_encoding data
        |> begin function
             | Error e -> error (Encoding_error e)
             | Ok bytes -> ok bytes
           end
        >>? fun bytes ->
            Bytes.cat packed_data_prefix bytes |> ok)

  let pack_data_typed' ~ctxt data typ =
    Lwt.map Env.wrap_tzresult (
        let open Protocol.Script_ir_translator in
        let open Protocol.Environment.Error_monad in
        let legacy = true in
        let tr data = Michelson_v1_parser.(data.unexpanded) in
        Protocol.Michelson_v1_primitives.prims_of_strings (tr typ) |> Lwt.return
        >>=? fun typ ->
        let (typ, node) = (Micheline.root typ, Micheline.root data) in
        parse_packable_ty ctxt ~legacy typ |> Lwt.return
        >>=? fun (Ex_ty typ, ctxt) ->
        parse_data ctxt ~allow_forged:true ~legacy typ node
        >>=? fun (data, ctxt) ->
        Protocol.Script_ir_translator.pack_data ctxt typ data
        >>=? fun (bytes, _ctxt) -> return bytes)

  let pack_data' ?typ data =
    match typ with
    | None -> pack_data_untyped' data
    | Some typ ->
       fake_alpha_context >>=? fun (ctxt,_,_) ->
       pack_data_typed' ~ctxt data typ

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
        Michelson_v1_parser.(typ.unexpanded)
        |>  Protocol.Michelson_v1_primitives.prims_of_strings |> Lwt.return
        >>=? fun typ ->
        parse_packable_ty ctxt ~legacy (Micheline.root typ) |> Lwt.return
        >>=? fun (Ex_ty typ, ctxt) ->
        parse_data ctxt ~allow_forged:true ~legacy typ
          (Micheline.root node)
        >>=? fun (x, ctxt) ->
        unparse_data ctxt Readable typ x
        >>=? fun (node, _ctxt) ->
        Micheline.strip_locations node |> return
      end >|= Env.wrap_tzresult

  let unpack_data ?typ (data : bytes) =
    match typ with
    | None -> unpack_data_untyped data
    | Some typ ->
       fake_alpha_context >>=? fun (ctxt,_,_) ->
       unpack_data_typed ~ctxt data typ

  module Handy = struct

    let pack_data ?typ data =
      let open Em in
      let progn =
        (match typ with
         | None -> return None
         | Some typ -> Lwt.return (parse_expr typ) >|=? Option.some)
        >>=? fun typ ->
        Lwt.return (parse_expr data) >>=? fun data ->
        pack_data ?typ data in
      progn >|= strip_tzresult
        

    let unpack_data ?typ data =
      let progn =
        unpack_data ?typ data >>=? fun expr ->
        string_of_expr expr |> return in
      progn >|= strip_tzresult

    let hash_expr ~typ expr =
      let progn = 
        fake_alpha_context >>=? fun (ctxt,_,_) ->
        Lwt.return (parse_expr typ) >>=? fun typ ->
        Lwt.return (parse_expr expr) >>=? fun expr ->
        hash_parsed_expr ~ctxt expr typ in
      progn >|= strip_tzresult

  end
end
