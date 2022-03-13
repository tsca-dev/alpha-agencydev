open Indexerlib

open Tzutils

open Tezos_shell_services
open Tezos_protocol_plugin_011_PtHangz2

module Protocol_error_monad = Protocol.Environment.Error_monad
module Alpha_services = Tezos_protocol_011_PtHangz2.Protocol.Alpha_services
module Alpha_context = Tezos_protocol_011_PtHangz2.Protocol.Alpha_context

module BlockServices = Tezos_client_011_PtHangz2.Protocol_client_context.Alpha_block_services
module ContextServices = BlockServices.Context
module HelpersServices = BlockServices.Helpers

open Lwt.Infix

module RpcHelper(Conf : sig
             val cctxt : Tezos_client_011_PtHangz2.Protocol_client_context.full
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
    block_header_m ?chain ?block () >|= strip_tzresult

  let contract_balance_m ?chain ?block contract =
    let cctxt, chain, block = confopts ~chain ~block in
    let open Tezos_base__TzPervasives in
    Tznode.get_contract cctxt contract  >>=? fun (_, contract) ->
    Alpha_services.Contract.balance cctxt (chain, block) contract

  let contract_balance ?chain ?block contract =
    contract_balance_m ?chain ?block contract >|= strip_tzresult

  let contract_storage_m ?chain ?block contract =
    let cctxt, chain, block = confopts ~chain ~block in
    let open Tezos_base__TzPervasives in
    Tznode.get_contract cctxt contract  >>=? fun (_, contract) ->
    Plugin.RPC.Contract.get_storage_normalized cctxt (chain, block) ~contract
      ~unparsing_mode:Optimized_legacy
    >>=? function
    | Some storage -> Tzutils.Michelson.string_of_expr storage |> return
    | None -> raise Not_found

  let contract_storage ?chain ?block contract =
    contract_storage_m ?chain ?block contract >|= strip_tzresult

  let bigmap_val_m ?chain ?block bigmap key key_type =
    let cctxt, chain, block = confopts ~chain ~block in
    Tzutils.Michelson.Handy.hash_expr ~typ:key_type key >>= fun hash ->
    (* Alpha_services.Contract.big_map_get cctxt (chain, block) bigmap hash >>= fun expr -> *)
    Plugin.RPC.Big_map.big_map_get_normalized cctxt (chain, block)
      bigmap hash
      ~unparsing_mode:Optimized_legacy


  let bigmap_val ?chain ?block bigmap key key_type =
    bigmap_val_m ?chain ?block bigmap key key_type
    >|= strip_tzresult
    >|= Tzutils.Michelson.string_of_expr
end

module BigmapId = Alpha_context.Big_map.Id


let fresh_index
      ?logger:(logger=output_string stderr)
      ?chain:(chain=`Main) ?block:(block=`Head 0)
      ~bridge:((module Bridge) : Indexerlib.bridge)
      ~brokers
      cctxt  =
  let log fmt = Format.kasprintf logger fmt in
  let open RpcHelper(struct let cctxt = cctxt and chain = chain and block = block end) in
  let open IndexedTypes in
  let index_broker broker =
    let open Kxclib in
    (* let open Brokerlib in *)
    let open IndexedTypes in
    (* TODO - check broker version *)
    let open Bridgelib.Bridge_commands in
    (contract_storage broker
     >|= Bridge.of_michelson Broker_store_data
     >|= Result.fold ~ok:(Conv.of_broker_store' ~broker)
           ~error:(fun e ->
             Log0.error "Bridge.of_michelson Broker_store_data : %a" pp_exn e;
             failwith "panic")
     >|= Fn.tap ((log "broker %s : @;%a" broker Sexplib.Sexp.pp_hum) %
                   sexp_of_broker_entry)) >>= fun store ->
    let ntmpl = store.idxbrk_template_count
    and nsprt = store.idxbrk_spirit_count in
    let natkey_bigmap_get bigmapid x =
      bigmap_val (Z.of_int bigmapid |> BigmapId.parse_z) (string_of_int x) "nat" in
    let template_descriptor = natkey_bigmap_get store.idxbrk_template_bigmapid in
    let spirit_descriptor = natkey_bigmap_get store.idxbrk_spirit_bigmapid in
    (iota ntmpl |&> (fun tmplid ->
       template_descriptor tmplid
       >|= Bridge.of_michelson Template_descriptor_data
       >|= Result.get_ok
       >|= Conv.of_template_descriptor ~broker ~tmplid
       >|= Fn.tap ((log "template #%d : @;%a" tmplid Sexplib.Sexp.pp_hum) %
                     sexp_of_template_entry)) |> Lwt.all
     >|= Array.of_list) >>= fun templates ->
    let tmplhash_lookup tmplid = templates.(tmplid).idxtmpl_tmplhash in
    (iota nsprt |&> (fun sprtid ->
       spirit_descriptor sprtid
       >|= Bridge.of_michelson Spirit_descriptor_data
       >|= Result.get_ok
       >|= Conv.of_spirit_descriptor ~broker ~tmplhash_lookup
       >|= Fn.tap ((log "spirit #%d : @;%a" sprtid Sexplib.Sexp.pp_hum) %
                     sexp_of_spirit_entry))
     |> Lwt.all) >>= fun spirits ->
    (spirits |&> (fun sprt -> sprt.idxsprt_ensemble |&> fun x -> sprt, x)
     |> List.flatten |&> (fun (sprt, (rclabel, ktaddr)) ->
       contract_storage ktaddr >>= fun store ->
       let wstore, wstore_unpacked =
         Bridge.parse_wrapper_store store |> Result.get_ok in
       contract_balance ktaddr >>= fun balance ->
       let balance = Alpha_context.Tez.to_mutez balance in
       {
         idxav_address = ktaddr;
         idxav_broker = broker;
         idxav_sprthash = sprt.idxsprt_sprthash;
         idxav_rclabel = rclabel;
         idxav_balance = balance;
         idxav_wstore = wstore;
         idxav_wstore_unpacked = some wstore_unpacked;
       }
       |> Fn.tap ((log "avatar %s @%s = @;%a" rclabel sprt.idxsprt_sprthash
                     Sexplib.Sexp.pp_hum) %
                    sexp_of_avatar_entry)
       |> Lwt.return)
     |> Lwt.all) >>= fun avatars ->
    Lwt.return (store, templates, spirits, avatars) in
  (brokers |&> index_broker |> Lwt.all
   >|=
     List.fold_left (fun (brokers, templates, spirits, avatars) single_broker ->
         let (broker, btemplates, bspirits, bavatars) = single_broker in
         (broker :: brokers,
          (Array.to_list btemplates) @ templates,
          bspirits @ spirits,
          bavatars @ avatars)
       ) ([], [], [], []))
  >>= fun (brokers, templates, spirits, avatars) ->
  let open IndexV0 in
  let open Protocol.Environment in
  (block_header () >>= fun header -> begin
       header.chain_id |> Chain_id.to_b58check,
       header.hash |> Block_hash.to_b58check,
       header.shell.timestamp,
       header.shell.level
     end |> Lwt.return)
  >>= fun (chain_id, blockhash, block_timestamp, block_level) ->
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
  Lwt.return index
