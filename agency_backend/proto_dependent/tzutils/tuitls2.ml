open Tezos_base__TzPervasives
open Tezos_protocol_006_PsCARTHA
open Tezos_client_006_PsCARTHA
open Tezos_client_base
(* open Tezos_shell_services *)
open Tezos_micheline

open Protocol
open Alpha_context

(* open Client_keys
 * open Client_proto_args *)
open Protocol_client_context

module Injection = Reworked_injection

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

(* the defaults below copied from proto_006_PsCARTHA/lib_client/client_proto_args.ml *)
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
      minimal_nanotez_per_byte = default_minimal_nanotez_per_byte;
      minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit;
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
  let logger = match logger with
    | `Null -> null_logger
    | `Full ppf -> full_logger ppf
    | `FullStderr -> full_logger Format.err_formatter
  in
  { host; port; tls; logger }

let home = try Sys.getenv "HOME" with Not_found -> "/root"
let default_base_dir = Filename.concat home ".tezos-client"
let default_mockup_base_dir = Filename.concat home ".tezos-mockup-client-dir.tsca-broker"

let mk_client_context ?base_dir:(base_dir=default_base_dir) rpc_config : #full tzresult lwt =
  let open Tezos_client_base_unix in
  (* Make sure that base_dir is not a mockup. *)
  ( match Tezos_mockup.Persistence.classify_base_dir base_dir with
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

let mk_mockup_client_context
      ?base_dir:(base_dir=default_mockup_base_dir) () : #full tzresult lwt =
  let open Tezos_client_base_unix in
  let in_memory_mockup() =
    Tezos_mockup.Persistence.default_mockup_context ()
  in
  let base_dir_class = Tezos_mockup.Persistence.classify_base_dir base_dir in
  ( match base_dir_class with
  | Tezos_mockup.Persistence.Base_dir_is_empty
  | Tezos_mockup.Persistence.Base_dir_is_file
  | Tezos_mockup.Persistence.Base_dir_is_nonempty
  | Tezos_mockup.Persistence.Base_dir_does_not_exist ->
      let mem_only = true in
      in_memory_mockup () >>=? fun res -> return (res, mem_only)
  | Tezos_mockup.Persistence.Base_dir_is_mockup ->
      let mem_only = false in
      Tezos_mockup.Persistence.get_mockup_context_from_disk ~base_dir
      >>=? fun (((module Mockup_environment), _) as res) ->
      return (res, mem_only))
  >>=? fun ((mockup_env, rpc_context), mem_only) ->
  new Client_context_unix.unix_mockup ~base_dir ~mem_only ~mockup_env ~rpc_context
  |> new Protocol_client_context.wrap_full
  |> return

let strip_tzresult_lwt = BrokerUtils.strip_tzresult_lwt
let strip_tzresult = BrokerUtils.strip_tzresult

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
  |> strip_tzresult_lwt

let local_client_context ?base_dir ?logger () =
  client_context ?base_dir ?endpoint:None ?logger ()

let mockup_client_context ?base_dir () =
  mk_mockup_client_context ?base_dir () |> strip_tzresult_lwt

let get_contract cctxt str =
  Client_proto_contracts.ContractAlias.get_contract cctxt str >>= function
  | Ok c -> return c
  | Error k_errs -> (
    match Contract.of_b58check str with
    | Error _ as err ->
        Lwt.return (Environment.wrap_error err
                    |> function
                      | Ok x -> x
                      | Error c_errs ->
                         Error (k_errs @ c_errs))
        |> trace (failure "bad contract notation")
    | Ok s ->
        return (str, s))

type simulation_result =
  | SimFailed of {
      reason : string;
      trace  : trace;
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
    ?(entrypoint = "default") ?arg ~amount ?fee ?gas_limit ?storage_limit
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
    Option.unopt_map
      ~f:Script.lazy_expr
      ~default:Script.unit_parameter
      parameters
  in
  let contents = Transaction {amount; parameters; destination; entrypoint} in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?branch
    ~source
    ?fee
    ?gas_limit
    ?storage_limit
    ~src_pk
    ~src_sk
    ~fee_parameter
    contents
  >>=? fun ((_oph, _op, result) as res) ->
  Lwt.return (Injection.originated_contracts (Single_result result))
  >>=? fun contracts -> return (res, contracts)
  >>= report_michelson_errors
        ~no_print_source:true
        ~msg:"transfer simulation failed"
        cctxt
  >>= function None -> return_unit | Some (_res, _contracts) -> return_unit
