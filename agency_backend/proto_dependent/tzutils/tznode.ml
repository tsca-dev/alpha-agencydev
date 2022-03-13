open Tezos_base__TzPervasives
open Tezos_protocol_011_PtHangz2
open Tezos_client_011_PtHangz2
(* open Tezos_protocol_plugin_011_PtHangz2 *)
open Tezos_client_base
open Tezos_micheline

open Protocol
open Alpha_context

(* open Client_keys
 * open Client_proto_args *)
open Protocol_client_context

module Alpha_context = Alpha_context
module Injection = New_injection

type 'a lwt = 'a Lwt.t

let () =
  Client_keys.register_signer (module Tezos_signer_backends.Unencrypted)

let report_michelson_errors ?(no_print_source = false) ~msg
    (cctxt : #Client_context.printer) = function
  | Error errs ->
      cctxt#warning
        "%a"
        (Michelson_v1_error_reporter.report_errors
           ~details:(not no_print_source)
           ~show_source:(not no_print_source)
           ?parsed:None)
        errs
      >>= fun () -> cctxt#error "%s" msg >>= fun () -> Lwt.return_none
  | Ok data ->
      Lwt.return_some data

let parse_expression arg =
  Lwt.return
    (Micheline_parser.no_parsing_error
       (Michelson_v1_parser.parse_expression arg))

(* the defaults below copied from proto_007_PsDELPH1/lib_client/client_proto_args.ml *)
let default_minimal_fees =
  match Tez.of_mutez 100L with None -> assert false | Some t -> t
let default_minimal_nanotez_per_gas_unit = Z.of_int 100
let default_minimal_nanotez_per_byte = Z.of_int 1000
let default_fee_cap = Tez.of_string "1.0" |> Stdlib.Option.get
let default_burn_cap = Tez.of_string "0.0" |> Stdlib.Option.get

let mk_fee_parameter
      ?force_low_fee:(force_low_fee=false)
      ?fee_cap:(fee_cap=default_fee_cap)
      ?burn_cap:(burn_cap=default_burn_cap)
      () = Injection.{
      minimal_fees = default_minimal_fees;
      minimal_nanotez_per_byte =  Q.zero;
      minimal_nanotez_per_gas_unit = Q.zero;
      force_low_fee;
      fee_cap;
      burn_cap;
    }

let mk_rpc_config ~host ~port ~tls :
      logger:[ `Null
             | `Full of Format.formatter
             | `FullStderr ] ->
      Tezos_rpc_http_client_unix.RPC_client_unix.config
  = fun ~logger ->
  let open Tezos_rpc_http_client_unix.RPC_client_unix in
  let endpoint =
    sprintf "%s://%s:%d" (if tls then "https" else "http") host port
    |> Uri.of_string in
  let logger = match logger with
    | `Null -> null_logger
    | `Full ppf -> full_logger ppf
    | `FullStderr -> full_logger Format.err_formatter
  in
  { endpoint; logger }

let home = try Sys.getenv "HOME" with Not_found -> "/root"
let default_base_dir = Filename.concat home ".tezos-client"

let mk_client_context ?base_dir:(base_dir=default_base_dir) rpc_config : #full tzresult lwt =
  let open Tezos_client_base_unix in
  (* Make sure that base_dir is not a mockup. *)
  Tezos_mockup.Persistence.classify_base_dir base_dir >>=? (function
  | Base_dir_is_mockup ->
      failwith
        "%s is setup as a mockup, yet mockup mode is not active"
        base_dir
  | _ ->
      return_unit )
  >>=? fun () ->
  let client = new Client_context_unix.unix_full
       ~chain:Client_config.default_chain
       ~block:Client_config.default_block
       ~confirmations:None
       ~password_filename:None
       ~base_dir
       ~rpc_config in
   new Protocol_client_context.wrap_full client |> return


open Tzutils

let parse_endpoint str =
  let orig = str in
  let starts_with p = Kxclib.String.starts_with p str in
  let chop p str = Kxclib.(String.chop_prefix p str |> Option.get) in
  let http, https = "http://", "https://" in
  let tls, str =
    if starts_with http then false, chop http str
    else if starts_with https then true, chop https str
    else false, str in
  let host, str =
    match Kxclib.String.split_on_char ':' str with
    | [ host ] -> host, ""
    | [ host; str ] -> host, str
    | _ -> invalid_arg ("invalid endpoint - "^orig) in
  let port =
    match Kxclib.String.split_on_char '/' str with
    | [ port ] -> port
    | [ port;  ""] -> port
    | _ -> invalid_arg ("endpoint with context path not supported - "^orig) in
  let port = int_of_string port in
  host, port, tls

let client_context ?base_dir ?endpoint ?logger:(logger=`FullStderr) () =
  let host, port, tls = match endpoint with
  | None -> "localhost", 8732, false
  | Some endp -> parse_endpoint endp in
  mk_rpc_config ~logger ~host ~port ~tls
  |> mk_client_context ?base_dir
  >|= strip_tzresult

let local_client_context ?base_dir ?logger () =
  client_context ?base_dir ?endpoint:None ?logger ()

let get_contract cctxt str =
  Client_proto_contracts.ContractAlias.get_contract cctxt str >>= function
  | Ok c -> return c
  | Error _errs -> (
    match Contract.of_b58check str with
    | Error _ as err ->
       Env.wrap_tzresult err |> Lwt.return
    | Ok s ->
        return (str, s))

type simulation_result =
  | SimFailed of {
      reason : string;
      trace  : error trace;
    }
  | SimSucceeded of {
    fee  : Tez.t;
    burn : Tez.t;
    report : string;
  }
type injection_result =
  | InjSucceeded(*  of {
     *   
     * } *)

type rich_operation =
  < adjusted : bool;
    simulate : simulation_result lwt;
    unsigned_bytes : bytes;
    signed_bytes : bytes*bytes -> bytes lwt; (* (unsigned, signature) -> bytes *)
    inject : bytes -> bytes lwt;
     >

let transfer (cctxt : #full) ?confirmations ?dry_run
    ?verbose_signing ?branch ~source ~destination
    ?(entrypoint = "default") ?arg ~amount ?fee
    ?gas_limit ?storage_limit
    ?force_low_fee ?fee_cap ?burn_cap ?chain ?block
    () =
  get_contract cctxt source >>=? fun (_, source) ->
  get_contract cctxt destination >>=? fun (_, destination) ->
  let source = Contract.is_implicit source |> Stdlib.Option.get in
  let fee_parameter = mk_fee_parameter ?force_low_fee ?fee_cap ?burn_cap () in
  let chain, block = Kxclib.Option.(
      chain |> v' (fun () -> cctxt#chain),
      block |> v' (fun () -> cctxt#block)) in
  Client_keys.get_key cctxt source
  >>=? fun (_, src_pk, src_sk) ->
  ( match arg with
  | Some arg ->
      parse_expression arg >>=? fun {expanded = arg; _} -> return_some arg
  | None ->
      return_none )
  >>=? fun parameters ->
  let parameters =
    Option.(
      map Script.lazy_expr parameters
      |> value ~default:Script.unit_parameter)
  in
  let contents = Transaction {amount; parameters; destination; entrypoint} in
  let fee = Limit.of_option fee in
  let gas_limit = Limit.of_option gas_limit in
  let storage_limit = Limit.of_option storage_limit in
  let contents =
    Annotated_manager_operation.
    Single_manager
      (Injection.prepare_manager_operation ~fee ~gas_limit ~storage_limit contents) in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?branch
    ~source
    ~fee
    ~gas_limit
    ~storage_limit
    ~src_pk
    ~src_sk
    ~fee_parameter
    contents
  >>=? fun ((_oph, _op, result) as res) ->
  Lwt.return (Injection.originated_contracts result)
  >>=? fun contracts -> return (res, contracts)
  >>= report_michelson_errors
        ~no_print_source:true
        ~msg:"transfer simulation failed"
        cctxt
  >>= function None -> return_unit | Some (_res, _contracts) -> return_unit

let simulate (cctxt : #full)
    ?branch ~source ~destination ?counter
    ?(entrypoint = "default") ?arg ~amount ?fee ?gas_limit ?storage_limit
    (* ?force_low_fee ?fee_cap ?burn_cap *)
    ?chain ?block
    () =
  (get_contract cctxt source >>=? fun (_, source) ->
  get_contract cctxt destination >>=? fun (_, destination) ->
  let source = Contract.is_implicit source |> Stdlib.Option.get in
  (* let fee_parameter = mk_fee_parameter ?force_low_fee ?fee_cap ?burn_cap () in *)
  let chain, block = Kxclib.Option.(
      chain |> v' (fun () -> cctxt#chain),
      block |> v' (fun () -> cctxt#block)) in
  ( match arg with
  | Some arg ->
      parse_expression arg >>=? fun {expanded = arg; _} -> return_some arg
  | None ->
      return_none )
  >>=? fun parameters ->
  let parameters =
    Option.(
      map Script.lazy_expr parameters
      |> value ~default:Script.unit_parameter)
  in
  Alpha_services.Constants.all cctxt (chain, block) >>=? fun {
      parametric =
        {
          hard_gas_limit_per_operation = gas_limit0;
          hard_storage_limit_per_operation = storage_limit0;
          _;
        };
      _;      
    } ->
  let contents = Transaction {amount; parameters; destination; entrypoint} in
  let limit_or_zero zero o = o |> Kxclib.Option.v zero |> Limit.known  in
  let fee = limit_or_zero Tez.zero fee in
  let gas_limit = limit_or_zero gas_limit0 gas_limit in
  let storage_limit = limit_or_zero storage_limit0 storage_limit in
  (* let apply_specified_options (\* counter *\) op = *)
  (*   Annotated_manager_operation.set_source source op >>? fun op -> *)
  (*   (\* Annotated_manager_operation.set_counter counter op >>? fun op -> *\) *)
  (*   Annotated_manager_operation.join_fee fee op >>? fun op -> *)
  (*   Annotated_manager_operation.join_gas_limit gas_limit op >>? fun op -> *)
  (*   Annotated_manager_operation.join_storage_limit storage_limit op *)
  (* in *)
  (match counter with
  | None ->
      Alpha_services.Contract.counter cctxt (chain, block) source
      >>=? fun pcounter ->
      let counter = Z.succ pcounter in
      return counter
  | Some counter -> return counter)
  >>=? fun counter ->
  Annotated_manager_operation.(
    Single_manager
      (Injection.prepare_manager_operation ~counter ~source ~fee ~gas_limit ~storage_limit contents)
    |> manager_list_from_annotated
    |> Lwt.return
  ) >>=? fun contents ->
  (* Client_keys.get_key cctxt source *)
  (* >>=? fun (_, src_pk, _src_sk) -> *)
  Injection.simulate
    cctxt
    ~chain
    ~block
    ?branch
    contents
  >>=? fun (logs, (oph, op, result, (watermark, bytes))) ->
  Lwt.return (Injection.originated_contracts result.contents)
  >>=? fun contracts ->
  let watermark = watermark |> Signature.bytes_of_watermark in
  return (logs, oph, (watermark, bytes), contracts, op, result))
  >>= (function
       | Error errs ->
          let no_print_source = true in
          let errmsg =
            Format.asprintf "%a"
            (Michelson_v1_error_reporter.report_errors
               ~details:(not no_print_source)
               ~show_source:(not no_print_source)
               ?parsed:None)
            errs in
          `Failed errmsg |> Lwt.return
       | Ok data ->
          Lwt.return (`Succeeded data))

let get_branch
      ?chain ?block ?branch
      (cctxt : #full) =
  let chain, block = Kxclib.Option.(
      chain |> v' (fun () -> cctxt#chain),
      block |> v' (fun () -> cctxt#block)) in
  Injection.get_branch ~chain ~block cctxt branch

let decode_signature = function
  | `Base58check s -> Signature.of_b58check_exn s
  | `Binary b ->
     Data_encoding.Binary.of_bytes_exn
       Signature.encoding b
  | `HexOrBase58check s -> begin
      match Signature.of_b58check_opt s with
      | Some s -> s
      | None ->
         Commons.bytes_of_hex s
         |> Data_encoding.Binary.of_bytes_exn Signature.encoding
    end

let inject (cctxt : #full)
      ?chain ?block
      ~unsigned ~signature () =
  let chain, block = Kxclib.Option.(
      chain |> v' (fun () -> cctxt#chain),
      block |> v' (fun () -> cctxt#block)) in
  let open MonadOps(Kxclib.Option) in
  let signature = decode_signature signature in
  Injection.inject cctxt ~chain ~block ~unsigned ~signature >>=? fun (ophash, contracts) ->
  return (ophash |> Operation_hash.to_b58check, contracts |&> Contract.to_b58check)

