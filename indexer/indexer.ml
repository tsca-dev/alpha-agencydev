open Tezos_shell_services
open Indexerlib

open Tzutils

module Protocol_error_monad = Protocol.Environment.Error_monad
module Alpha_services = Tezos_protocol_007_PsDELPH1.Protocol.Alpha_services
module Alpha_context = Tezos_protocol_007_PsDELPH1.Protocol.Alpha_context

module BlockServices = Tezos_client_007_PsDELPH1.Protocol_client_context.Alpha_block_services
module ContextServices = BlockServices.Context
module HelpersServices = BlockServices.Helpers

module RpcHelper(Conf : sig
             val cctxt : Tezos_client_007_PsDELPH1.Protocol_client_context.full
             val chain : Block_services.chain
             val block : Block_services.block
           end) = struct
  let confopts ~chain ~block =
    let chain = Option.v Conf.chain chain
    and block = Option.v Conf.block block in
    Conf.cctxt, chain, block

  let block_header_m ?chain ?block () =
    let cctxt, chain, block = confopts ~chain ~block in
    BlockServices.header cctxt ~chain ~block ()

  let block_header ?chain ?block () =
    block_header_m ?chain ?block () |> strip_tzresult_lwt

  let contract_balance_m ?chain ?block contract =
    let cctxt, chain, block = confopts ~chain ~block in
    let open Tezos_base__TzPervasives in
    Tznode.get_contract cctxt contract  >>=? fun (_, contract) ->
    Alpha_services.Contract.balance cctxt (chain, block) contract

  let contract_balance ?chain ?block contract =
    contract_balance_m ?chain ?block contract |> strip_tzresult_lwt

  let contract_storage_m ?chain ?block contract =
    let cctxt, chain, block = confopts ~chain ~block in
    let open Tezos_base__TzPervasives in
    Tznode.get_contract cctxt contract  >>=? fun (_, contract) ->
    Alpha_services.Contract.storage cctxt (chain, block) contract >>=? fun storage ->
    Tzutils.Michelson.string_of_expr storage |> return

  let contract_storage ?chain ?block contract =
    contract_storage_m ?chain ?block contract |> strip_tzresult_lwt

  let bigmap_val_m ?chain ?block bigmap key key_type =
    let cctxt, chain, block = confopts ~chain ~block in
    let hash = Tzutils.Michelson.Handy.hash_expr ~typ:key_type key in
    Alpha_services.Contract.big_map_get cctxt (chain, block) bigmap hash

  let bigmap_val ?chain ?block bigmap key key_type =
    bigmap_val_m ?chain ?block bigmap key key_type
    |> strip_tzresult_lwt
    |> Tzutils.Michelson.string_of_expr
end


let () =
  let open Kxclib.ArgOptions in
  let endpoint = get_option (StringOption "-endpoint") in
  let base_dir = get_option (StringOption "-base-dir") in
  let logger =
    if has_flag "-l" then `FullStderr else `Null in
  let cctxt : #Tezos_client_007_PsDELPH1.Protocol_client_context.full =
    if has_flag "-mockup" then (
      let base_dir = get_option_exn (StringOption "-mockup") in
      Tznode.mockup_client_context ~base_dir ()
    ) else (
      Tznode.client_context ?endpoint ?base_dir ~logger()
    ) in
  let chain, block =
    let open Bcutils in
    get_option (StringOption "-chain") |> Option.fold ~none:`Main ~some:chain_of_string,
    get_option (StringOption "-block") |> Option.fold ~none:(`Head 0) ~some:block_of_string in
  let open Protocol.Environment in
  let info fmt =
    let open Stdlib.Format in
    let ppf = err_formatter in
    fprintf ppf "[INFO] @[<hov 2>";
    kfprintf (fun ppf -> fprintf ppf "@]@.") ppf fmt in
  let open RpcHelper(struct let cctxt = cctxt and chain = chain and block = block end) in
  let chain_id, blockhash, block_timestamp, block_level =
    let header = block_header () in
    header.chain_id |> Chain_id.to_b58check,
    header.hash |> Block_hash.to_b58check,
    header.shell.timestamp,
    header.shell.level in
  info "base url  : %s" (cctxt#base |> Uri.to_string);
  info "chain id  : %s" chain_id;
  info "block level / hash : %ld / %s" block_level blockhash;
  info "timestamp : %a" Time.pp_hum block_timestamp;  
  (match get_option (StringOption "-ktstorage") with
   | Some kt ->
      contract_storage kt
      |> print_endline
   | None -> ());
  (match get_option (StringOption "-bigmap") with
   | Some bigmap ->
      let bigmap = Z.of_string bigmap in
      (match get_option_exn (StringOption "-get") |> String.split_on_char ':' with
       | [key; key_type] ->
          bigmap_val bigmap key key_type
          |> print_endline
       | _ -> failwith "-bigmap: invalid arguments")
   | None -> ());
  (match get_option (StringOption "-brokers") with
   | Some brokers ->
      let brokers = String.split_on_char ',' brokers in
      brokers |!> info "broker to index: %s";
      let index =
        Indexer_v0.fresh_index cctxt
          ~logger:(info "%s")
          ~chain ~block ~brokers in
      let marshaled = IndexV0.Serialization.to_bytes index in
      info "resulting index size : %d-bytes" (Bytes.length marshaled);
      (match get_option (OutChannelOption' "-out") with
       | None -> ()
       | Some (ch, desc) ->
          output_bytes ch marshaled;
          let desc = (
              match desc with
              | `StandardChannel -> "<stdout>"
              | `FileChannel path -> path) in
          info "index written to %s" desc)
   | None -> ());

