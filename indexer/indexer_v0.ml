open Indexerlib

open Tzutils

open Tezos_shell_services

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


let fresh_index
      ?logger:(logger=output_string stderr)
      ?chain:(chain=`Main) ?block:(block=`Head 0)
      ~brokers
      cctxt  =
  let log fmt = Format.kasprintf logger fmt in
  let open RpcHelper(struct let cctxt = cctxt and chain = chain and block = block end) in
  let open IndexedTypes in
  let index_broker broker =
    let open Kxclib in
    let open Brokerlib in
    let open IndexedTypes in
    (* TODO - check broker version *)
    let store =
      contract_storage broker
      |> store'_bridge#revert |> Option.get
      |> Conv.of_broker_store' ~broker
      |> Fn.tap ((log "broker %s : @;%a" broker Sexplib.Sexp.pp_hum) %
                   sexp_of_broker_entry)in
    let ntmpl = store.idxbrk_template_count
    and nsprt = store.idxbrk_spirit_count in
    let natkey_bigmap_get bigmapid x =
      bigmap_val (Z.of_int bigmapid) (string_of_int x) "nat" in
    let template_descriptor = natkey_bigmap_get store.idxbrk_template_bigmapid in
    let spirit_descriptor = natkey_bigmap_get store.idxbrk_spirit_bigmapid in
    let templates =
      iota ntmpl |&> (fun tmplid ->
        template_descriptor tmplid
        |> template_descriptor_bridge#revert |> Option.get
        |> Conv.of_template_descriptor ~broker ~tmplid
        |> Fn.tap ((log "template #%d : @;%a" tmplid Sexplib.Sexp.pp_hum) %
                     sexp_of_template_entry))
      |> Array.of_list in
    let tmplhash_lookup tmplid = templates.(tmplid).idxtmpl_tmplhash in
    let spirits =
      iota nsprt |&> (fun sprtid ->
        spirit_descriptor sprtid
        |> spirit_descriptor_bridge#revert |> Option.get
        |> Conv.of_spirit_descriptor ~broker ~tmplhash_lookup
        |> Fn.tap ((log "spirit #%d : @;%a" sprtid Sexplib.Sexp.pp_hum) %
                     sexp_of_spirit_entry)) in
    let avatars =
      spirits |&> (fun sprt -> sprt.idxsprt_ensemble |&> fun x -> sprt, x)
      |> List.flatten |&> (fun (sprt, (rclabel, ktaddr)) ->
        let store = contract_storage ktaddr in
        let wstore = (parse_wrapper_store store)#wstore in
        let balance = contract_balance ktaddr in
        let balance = Alpha_context.Tez.to_mutez balance in
        {
          idxav_address = ktaddr;
          idxav_broker = broker;
          idxav_sprthash = sprt.idxsprt_sprthash;
          idxav_rclabel = rclabel;
          idxav_balance = balance;
          idxav_wstore = wstore;
          idxav_wstore_unpacked =
            try Some (Tzutils.Michelson.Handy.unpack_data wstore)
            with _ -> None;
        }
        |> Fn.tap ((log "avatar %s @%s = @;%a" rclabel sprt.idxsprt_sprthash
                      Sexplib.Sexp.pp_hum) %
                     sexp_of_avatar_entry)) in
    store, templates, spirits, avatars in
  let brokers, templates, spirits, avatars =
    List.fold_left (fun (brokers, templates, spirits, avatars) broker ->
        let (broker, btemplates, bspirits, bavatars) = index_broker broker in
        (broker :: brokers,
         (Array.to_list btemplates) @ templates,
         bspirits @ spirits,
         bavatars @ avatars)
      ) ([], [], [], []) brokers in
  let open IndexV0 in
  let open Protocol.Environment in
  let chain_id, blockhash, block_timestamp, block_level =
    let header = block_header () in
    header.chain_id |> Chain_id.to_b58check,
    header.hash |> Block_hash.to_b58check,
    header.shell.timestamp,
    header.shell.level in
  let block_info = {
      idxbl_chain_id =  chain_id;
      idxbl_blockhash = blockhash;
      idxbl_level = block_level;
      idxbl_timestamp = Time.to_seconds block_timestamp;
    } in
  let make_index ?multival:(mv=false) dataset keyfun =
    let indexing = Array.of_list dataset in
    let table = Hashtbl.create (Array.length indexing) in
    let insert = match mv with
      | true -> Hashtbl.add table
      | false -> Hashtbl.replace table in
    indexing,
    ((indexing |> Array.iteri (fun xid e ->
                      insert (keyfun e) xid));
     table) in
  let brokers, brkindex =
    make_index brokers (fun e -> e.idxbrk_address) in
  let templates, tmplindex =
    make_index ~multival:true templates (fun e -> e.idxtmpl_tmplhash) in
  let spirits, sprtindex =
    make_index spirits (fun e -> e.idxsprt_sprthash) in
  let avatars, avindex =
    make_index avatars (fun e -> e.idxav_address) in
  let index = {
      block_info;
      brokers; templates; spirits; avatars;
      brkindex; tmplindex; sprtindex; avindex;
    } in
  index
