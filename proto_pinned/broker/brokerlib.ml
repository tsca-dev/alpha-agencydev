module Tzutils = Tzutils
module Tzcrypto = Tzcrypto

module%scamltypes TemplateTypes = struct
  open SCaml

  type ccgen = {
      genprog : (
        bytes*tz ->
        (** [genesis_parameter, init_balance_total] *)
        (string*wfunc*bytes*tz) list
        (** avatar ensemble, each (rclabel, wrapped_code, init_storage, init_balance) *)
      );
      (** property : [init_balance]'s of returned must sum to [init_balance_total] passed in *)

      initprog : (
        bytes*tz ->
        (** [genesis_parameter, init_balance_total] *)
        (string, address) map ->
        (** physical address of each avatar, as a finmap of [rclabel |-> address] *)
        (string*bytes) list
        (** initialization message per contract, as an assoc_map of [rclabel, init_message] *)
      );
    }
  (** spirit (= "conceptual contract") generator *)
  [@@scaml.noconv]

  and wfunc = bytes*bytes -> operation list*bytes
  (** physical smart contract body type to be wrapped in the wrapper *)
  [@@scaml.noconv]

  and wrapper_store = {
      wrapped_storage : bytes;
      wrapped_wfunc : wfunc;
      wrapped_identity : avatar_identity option;
    }
  [@@scaml.noconv]

  and avatar_identity = {
      avid_broker   : address;
      avid_sprthash : string;
      avid_rclabel  : string;
    }

  let dummy_wfunc : wfunc = fun (_, wstore) -> [], wstore
end

module%scamltypes BrokerTypes = struct
  open SCaml

  type fee_descriptor =
    | ChargeFree
    | ChargeOf of {
        charge_amount : tz;
        charge_collector : key_hash;
      }

  type template_descriptor = {
      tmpldesc_ccgen : bytes;
      (** must unpack to type [ccgen] *)
      tmpldesc_tmplhash : bytes;
      (** sha256 of [ccgen] that uniquely identity the template code *)

      tmpldesc_fee : fee_descriptor;
      tmpldesc_availability : bool;
      tmpldesc_bookhash : string option;

      provider_revenue_collectable : tz;
    }

  type genesis_request = {
      genreq_sprthash : string;
      genreq_tmplid : nat;
      genreq_tmplhash : bytes;
      genreq_genparam : bytes;
      genreq_initbalance : tz;

      genreq_broker_fee : tz;
      genreq_provider_fee : tz;
    }

  type spirit_descriptor = {
      sprtdesc_sprthash : string;
      sprtdesc_requester : address;
      sprtdesc_tmplid : nat;

      sprtdesc_ensemble : (string, address) map; (** [rclabel |-> address] *)
    }

  type broker_store = {
      broker_version : string;

      broker_fee : fee_descriptor;
      broker_admins : address set;
      broker_availability : bool;
      broker_banner : string;
      (** arbitrary string for informative purposes *)

      templates : (nat, template_descriptor) big_map*nat;
      (** [(tmplid |-> template_descriptor), next_id] *)
      instances : (nat, spirit_descriptor) big_map*nat;
      (** [(sprtid |-> spirit_descriptor), next_id] *)
      instance_index : (string, nat) big_map;
      (** [sprthash |-> sprtid] *)

      broker_revenue_collectable : tz;
    }

  (* it is intentional not providing a way to collect
     exceeded funds passed to the broker contract
     - those are effectively burned *)
  type broker_message =
    | TscaBrokerVersion

    | GenesisRequest of {
        sudo : bool;
        genreq : genesis_request;
      }

    | ProviderRevenueCollection of {
        revenue_sources : nat set;
        (** set of [tmplid] *)
        collector : key_hash;
        (** beneficiary of the withdraw transfer *)
      }
    | BrokerRevenueCollection of {
        collector : key_hash;
        (** beneficiary of the withdraw transfer *)
      }

    | SudoCollectProviderRevenue of {
        revenue_sources : nat set;
        (** set of [tmplid] *)
        collector : key_hash;
        (** beneficiary of the withdraw transfer *)
        reason : string option;
        (** arbitrary string for informative purposes *)
      }
    | SudoCollectBrokerRevenue of {
        collector : key_hash;
        (** beneficiary of the withdraw transfer *)
        reason : string option;
        (** arbitrary string for informative purposes *)
      }

    | SudoUploadTemplate of {
        tmplid : nat;
        ccgen : bytes;
        tmplhash : bytes option;
        (** if provided, will be checked against [sha256 ccgen] *)
      }
    | SudoConfigTemplate of {
        tmplid : nat;
        fee_updates          : fee_descriptor option;
        availability_updates : bool option;
        bookhash_updates : string option option;
      }
    | SudoAddAdmins of address set
    | SudoRemoveAdmins of address set
    | SudoConfigBroker of {
        fee_updates          : fee_descriptor option;
        availability_updates : bool option;
        banner_updates       : string option;
      }
end

module%scamlcontract BrokerContract_VerProto03Dev = struct
  open SCaml
  open TemplateTypes
  open BrokerTypes

  let broker_version_proto03_dev = "v:tsca_proto03~dev"

  let version = broker_version_proto03_dev

  let get_principal() = Global.get_source()

  let fee = function
    | ChargeFree -> Tz 0.
    | ChargeOf c -> c.charge_amount

  let addr kh = Contract.(address (implicit_account kh))

  let succ_nat x = x +^ (Nat 1)

  let check_sudo_privilege bkst =
    let principal = get_principal() in
    if not (Set.mem principal bkst.broker_admins)
    then failwith ("not authorized", principal)

  let check_collector collector =
    let principal = get_principal() in
    if (addr collector) <> principal
    then failwith ("you must be the collector", principal, collector)

  let check_availability bkst =
    if not bkst.broker_availability
    then failwith "broker is not available"

  let check_genesis_amount genreq amount broker_fee provider_fee initbalance =
    let broker_fee, provider_fee = fee broker_fee, fee provider_fee in
    (if genreq.genreq_broker_fee <> broker_fee
     then failwith ("broker_fee unmatch", genreq.genreq_broker_fee, broker_fee));
    (if genreq.genreq_provider_fee <> provider_fee
     then failwith ("provider_fee unmatch", genreq.genreq_provider_fee, provider_fee));
    let minimal =
      (Tz 0.)
      +$ broker_fee
      +$ provider_fee
      +$ initbalance in
    if amount < minimal
    then failwith ("insufficient transaction amount",
                   (amount, minimal),
                   (broker_fee,
                    provider_fee,
                    initbalance))

  let fresh_tmpldesc ccgen tmplhash =
    let tmplhash_calc =
      let full = Crypto.sha256 ccgen in
      let chop_length = (Nat 16) in
      (* this value should be kept in-sync with
       * [Tzcrypto.Arbitrarity.tmplhash_scheme ~rawlen] *)
      match Bytes.slice (Nat 0) chop_length full with
      | None -> failwith "panic"
      | Some calc -> calc in
    let tmplhash = match tmplhash with
      | None -> tmplhash_calc
      | Some provided ->
         (if provided <> tmplhash_calc
          then failwith ("tmplhash does not match", tmplhash_calc, provided));
         tmplhash_calc in
    { tmpldesc_ccgen = ccgen;
      tmpldesc_tmplhash = tmplhash;

      tmpldesc_fee = ChargeFree;
      tmpldesc_availability = false;
      tmpldesc_bookhash = None;

      provider_revenue_collectable = Tz 0.; }

  let fresh_sprtdesc genreq ensemble = {
      sprtdesc_sprthash = genreq.genreq_sprthash;
      sprtdesc_requester = get_principal();
      sprtdesc_tmplid  = genreq.genreq_tmplid;
      sprtdesc_ensemble = ensemble;
    }

  let mk_transfer beneficiary amount =
    Operation.transfer_tokens () amount 
      (Contract.implicit_account beneficiary)

  let wrapped_contract identity body initst initbal : operation*address =
    let storage = {
        wrapped_storage = initst;
        wrapped_wfunc = body;
        wrapped_identity = identity;
      } in
    Contract.create_from_tz_file "wrapper.tz" None initbal storage

  let invoke_ccgen broker (tmplid : nat) sprthash ccgen_bytes
        (genparam : bytes) (baltot : tz) =
    (* unpack ccgen *)
    let ccgen = match (Obj.unpack ccgen_bytes : ccgen option) with
      | None -> failwith ("unable to unpack ccgen of template", tmplid)
      | Some ccgen -> ccgen in
    (* run genprog *)
    let ensemble_defs = ccgen.genprog (genparam, baltot) in
    let ops, ensemble, initbalsum =
      let ops : operations = []
      and ensemble : (string, address) map = Map.empty in
      List.fold_left' (fun ((ops, ensemble, initbalsum),
                            (rclabel, body, initst, initbal)) ->
          let identity = Some {
              avid_broker = broker;
              avid_sprthash = sprthash;
              avid_rclabel = rclabel;
            } in
          let op, ktaddr = wrapped_contract identity body initst initbal in
          let ensemble = Map.update rclabel (Some ktaddr) ensemble in
          op :: ops, ensemble, initbalsum +$ initbal)
        (ops, ensemble, Tz 0.) ensemble_defs in
    ((* check initbalsum = baltot *)
     if initbalsum <> baltot
     then failwith ("ccgen.genprog error - initial balance does not match", initbalsum, baltot));
    (* run initprog *)
    let init_calls = ccgen.initprog (genparam, baltot) ensemble in
    let ops =
      List.fold_left' (fun (ops, (rclabel, arg)) ->
          let addr = match Map.get rclabel ensemble with
            | None -> failwith ("ccgen.initprog - no such rclabel", rclabel)
            | Some addr -> addr in
          let kt = match (Contract.contract addr : bytes contract option) with
            | None -> failwith ("panic@ccgen.initprog kt.cast")
            | Some kt -> kt in
          let op = Operation.transfer_tokens arg (Tz 0.) kt in
          op :: ops)
        ops init_calls in
    (* return *)
    ensemble, ops

  let errmsg_tmplid_not_exists = "tmplid not exists"
  let errmsg_template_not_available = "template not available"
  let errmsg_spirit_already_exists = "spirit already exists"
  let errmsg_revenue_collection_no_permission = "no permission to collect revenue"

  let handle_genesis sudo broker genreq bkst =
       let { templates = (templates,nxtmplid);
             instances = (instances,nxsprtid);
             instance_index;
             broker_revenue_collectable = broker_revenue;
             broker_availability;
             _ } = bkst in
       let tmplid = genreq.genreq_tmplid in
       (match BigMap.get tmplid templates with
        | None -> failwith (errmsg_tmplid_not_exists, tmplid)
        | Some tmpldesc ->
           let { genreq_sprthash = sprthash;
                 genreq_genparam = genparam;
                 genreq_initbalance = initbalance;
                 genreq_broker_fee; genreq_provider_fee;
                 _ } = genreq in
           let { tmpldesc_tmplhash = tmplhash;
                 tmpldesc_availability = template_availability;
                 tmpldesc_ccgen = ccgen;
                 provider_revenue_collectable = provider_revenue;
                 _ } = tmpldesc in
           (* check template availability *)
           ((if (not sudo) && ((not broker_availability) || (not template_availability))
             then failwith (errmsg_template_not_available, tmplid, tmplhash)));
           (* check that sprthash does not exist yet *)
           (if BigMap.mem sprthash instance_index
            then failwith (errmsg_spirit_already_exists, sprthash));
           (* check genreq.*_fee and transaction amount *)
           let broker_fee, provider_fee =
             if sudo then ChargeFree, ChargeFree
             else bkst.broker_fee, tmpldesc.tmpldesc_fee in
           (let amount = Global.get_amount() in
            check_genesis_amount genreq amount broker_fee provider_fee initbalance);
           (* actual genesis handling logic *)
           let ensemble, ops = invoke_ccgen broker tmplid sprthash ccgen genparam initbalance in
           let sprtdesc = fresh_sprtdesc genreq ensemble in
           let instances = BigMap.update nxsprtid (Some sprtdesc) instances in
           let instance_index, nxsprtid =
             BigMap.update sprthash (Some nxsprtid) instance_index,
             succ_nat nxsprtid in
           (* fee collection *)
           let tmpldesc = {
               tmpldesc with
               provider_revenue_collectable = provider_revenue +$ genreq_provider_fee
             } in
           let templates = BigMap.update tmplid (Some tmpldesc) templates in
           let bkst = {
               bkst with
               instances = (instances, nxsprtid);
               templates = (templates,nxtmplid);
               instance_index;
               broker_revenue_collectable = broker_revenue +$ genreq_broker_fee
             } in
           ops, bkst)

  (* dev check-lists:
   * - every [Sudo*] message handler calls [check_sudo_priviledge] first *)

  let main (msg : broker_message) (bkst : broker_store) =
    let broker = Contract.(address self) in
    match msg with
    | TscaBrokerVersion ->
       failwith ("rt", version)

    | GenesisRequest { sudo; genreq } ->
       if sudo then check_sudo_privilege bkst;
       handle_genesis sudo broker genreq bkst

    | ProviderRevenueCollection { collector; revenue_sources; } ->
       check_collector collector;
       let templates, nxtmplid = bkst.templates in
       let amount, templates =
         Set.fold' (fun (tmplid,
                         (tot,templates)) ->
             (match BigMap.get tmplid templates with
              | None -> failwith (errmsg_tmplid_not_exists, tmplid)
              | Some ({ provider_revenue_collectable = am;
                        tmpldesc_fee = tmplfee; _ } as tmpldesc) ->
                 ((* check permission of collector *)
                  match tmplfee with
                  | ChargeOf { charge_collector; _ } when collector = charge_collector ->
                     () (* okay in this case *)
                  | _ -> failwith (errmsg_revenue_collection_no_permission,
                                   collector, tmplid, tmplfee));
                 let tmpldesc = { tmpldesc with provider_revenue_collectable = Tz 0. } in
                 tot +$ am,
                 BigMap.update tmplid (Some tmpldesc) templates))
           revenue_sources
           (Tz 0., templates) in
       let op = mk_transfer collector amount in
       let bkst = { bkst with templates = (templates, nxtmplid) } in
       [op], bkst

    | BrokerRevenueCollection { collector; } ->
       check_collector collector;
       ((* check permission of collector *)
        match bkst.broker_fee with
        | ChargeOf { charge_collector; _ } when collector = charge_collector ->
           () (* okay in this case *)
        | _ -> failwith (errmsg_revenue_collection_no_permission,
                         collector, bkst.broker_fee));
       let amount = bkst.broker_revenue_collectable in
       let op = mk_transfer collector amount in
       let bkst = { bkst with broker_revenue_collectable = Tz 0. } in
       [op], bkst

    | SudoCollectProviderRevenue { collector; revenue_sources; _ } ->
       check_sudo_privilege bkst;
       let templates, nxtmplid = bkst.templates in
       let amount, templates =
         Set.fold' (fun (tmplid,
                         (tot,templates)) ->
             (match BigMap.get tmplid templates with
              | None -> failwith (errmsg_tmplid_not_exists, tmplid)
              | Some ({ provider_revenue_collectable = am; _ } as tmpldesc) ->
                 (* note - we do not check collector for SudoCollectProviderRevenue *)
                 let tmpldesc = { tmpldesc with provider_revenue_collectable = Tz 0. } in
                 tot +$ am,
                 BigMap.update tmplid (Some tmpldesc) templates))
           revenue_sources
           (Tz 0., templates) in
       let op = mk_transfer collector amount in
       let bkst = { bkst with templates = templates, nxtmplid } in
       [op], bkst

    | SudoCollectBrokerRevenue { collector; _ } ->
       check_sudo_privilege bkst;
       let amount = bkst.broker_revenue_collectable in
       let op = mk_transfer collector amount in
       let bkst = { bkst with broker_revenue_collectable = Tz 0. } in
       [op], bkst

    | SudoUploadTemplate { tmplid; ccgen; tmplhash; } ->
       check_sudo_privilege bkst;
       let templates, nxtmplid = bkst.templates in
       (* check tmplid agrees *)
       (if tmplid <> nxtmplid
        then failwith ("tmplid <> next_tmplid", nxtmplid, tmplid));
       (* typecheck ccgen *)
       (match (Obj.unpack ccgen : ccgen option) with
        | None -> failwith ("ccgen does not typecheck")
        | Some _ -> ());
       let tmpldesc = fresh_tmpldesc ccgen tmplhash in
       let templates = BigMap.update nxtmplid (Some tmpldesc) templates in
       let bkst = { bkst with templates = templates, succ_nat nxtmplid } in
       [], bkst

    | SudoConfigTemplate {
        tmplid;
        fee_updates;
        availability_updates;
        bookhash_updates } ->
       check_sudo_privilege bkst;
       let templates, nxtmplid = bkst.templates in
       (match BigMap.get tmplid templates with
        | None -> failwith (errmsg_tmplid_not_exists, tmplid)
        | Some tmpldesc ->
           let tmpldesc = match fee_updates with
             | None -> tmpldesc
             | Some tmpldesc_fee -> { tmpldesc with tmpldesc_fee } in
           let tmpldesc = match availability_updates with
             | None -> tmpldesc
             | Some tmpldesc_availability -> { tmpldesc with tmpldesc_availability } in
           let tmpldesc = match bookhash_updates with
             | None -> tmpldesc
             | Some tmpldesc_bookhash -> { tmpldesc with tmpldesc_bookhash } in
           let templates = BigMap.update tmplid (Some tmpldesc) templates in
           let bkst = { bkst with templates = templates, nxtmplid } in
           [], bkst)

    | SudoAddAdmins additional_admins ->
       check_sudo_privilege bkst;
       let broker_admins = Set.fold (fun a admins -> Set.update a true admins)
                             additional_admins bkst.broker_admins in
       let bkst = { bkst with broker_admins } in
       [], bkst

    | SudoRemoveAdmins revoking_admins ->
       check_sudo_privilege bkst;
       let principal = get_principal() in
       (if Set.mem principal revoking_admins
        then failwith ("you cannot remove yourself as an admin", principal));
       let broker_admins = Set.fold (fun a admins -> Set.update a false admins)
                             revoking_admins bkst.broker_admins in
       ((* this check is unnecessary if we disallow one removing his/herself as
         * an admin, but kept here to prevent mistakes after changing that behavior *)
        if Set.length broker_admins = (Nat 0)
        then failwith "you cannot remove all admins");
       let bkst = { bkst with broker_admins } in
       [], bkst

    | SudoConfigBroker { fee_updates; availability_updates; banner_updates } ->
       check_sudo_privilege bkst;
       let bkst = match fee_updates with
         | None -> bkst
         | Some broker_fee -> { bkst with broker_fee } in
       let bkst = match availability_updates with
         | None -> bkst
         | Some broker_availability -> { bkst with broker_availability } in
       let bkst = match banner_updates with
         | None -> bkst
         | Some broker_banner -> {bkst with broker_banner} in
       [], bkst
  [@@entry]
end

module BrokerContract = BrokerContract_VerProto03Dev

module%scamltypes BrokerTypesHacks = struct
  open SCaml

  type avatar_identity = TemplateTypes.avatar_identity = {
      avid_broker   : address;
      avid_sprthash : string;
      avid_rclabel  : string;
    }

  (** using bytes instead of key_hash due to SCaml limitations *)
  type fee_descriptor =
    | ChargeFree
    | ChargeOf of {
        charge_amount : tz;
        charge_collector : bytes;
      }

  type template_descriptor = {
      tmpldesc_ccgen : bytes;
      (** must unpack to type [ccgen] *)
      tmpldesc_tmplhash : bytes;
      (** sha256 of [ccgen] that uniquely identity the template code *)

      tmpldesc_fee : fee_descriptor;
      tmpldesc_availability : bool;
      tmpldesc_bookhash : string option;

      provider_revenue_collectable : tz;
    }

  (** using bytes instead of address due to SCaml limitations *)
  type spirit_descriptor = {
      sprtdesc_sprthash : string;
      sprtdesc_requester : bytes;
      sprtdesc_tmplid : nat;

      sprtdesc_ensemble : (string, bytes) map;
      (** [rclabel |-> address] *)
    }

  type broker_store = {
      broker_version : string;

      broker_fee : fee_descriptor;
      broker_admins : bytes set; (** using bytes instead of address due to SCaml limitations *)
      broker_availability : bool;
      broker_banner : string;
      (** arbitrary string for informative purposes *)

      templates : nat*nat;
      instances : nat*nat;
      instance_index : nat;

      broker_revenue_collectable : tz;
    }
  (** this is a SCaml hack to help reading
      the contract storage containing big-map identifiers *)

  type maybe_avatar_identity = avatar_identity option
end

open Tzutils
open Commons

let tztez0 = SCaml.(Tz 0.)
let tznat0 = SCaml.(Nat 0)

let tzbytes bytes = SCaml.Bytes (hex_of_bytes bytes)
let tzset list = SCaml.Set list
let tzmap alist = SCaml.Map alist
let tzbigmap alist = SCaml.BigMap alist
let tzbigmap_empty() = SCaml.BigMap.empty
let tzaddr addr = SCaml.Address addr
let tznat x = SCaml.Nat x
let tzmutez x = SCaml.Tz ((Int64.to_float x) /. 1000000.)
let tztez x = SCaml.Tz (x)

let ocbytes (SCaml.Bytes hex) = bytes_of_hex hex
let ocnat (SCaml.Nat x) = x
let ocmutez (SCaml.Tz x) = x *. 1000000. |> Int64.of_float
let ockh (SCaml.Key_hash addr) = addr
let ocaddr (SCaml.Address addr) = addr
let ocmap (SCaml.Map alist) = alist
let ocset (SCaml.Set list) = list

type broker_init_store_opts = {
    admin : string;
    banner : string;
  }

let broker_init_store { admin; banner } = BrokerTypes.{
    broker_version = BrokerContract.version;
    broker_fee = ChargeFree;
    broker_admins = Set [Address admin];
    broker_availability = false;
    broker_banner = banner;
    templates = SCaml.BigMap.empty, tznat0;
    instances = SCaml.BigMap.empty, tznat0;
    instance_index = SCaml.BigMap.empty;
    broker_revenue_collectable = tztez0;
  }

type 'a tzbridge = <
    tztype : string;
    convert : 'a -> string;
    revert  : string -> 'a option;
  >

let _ =
  SCamlTyperep.to_michelson

let message_bridge =
  ([%scamltype: BrokerTypes.broker_message] :> _ tzbridge)

let store_bridge =
  ([%scamltype: BrokerTypes.broker_store] :> _ tzbridge)

let fee_descriptor_bridge =
  ([%scamltype: BrokerTypes.broker_store] :> _ tzbridge)

let store'_bridge =
  ([%scamltype: BrokerTypesHacks.broker_store] :> _ tzbridge)

let template_descriptor'_bridge =
  ([%scamltype: BrokerTypesHacks.template_descriptor] :> _ tzbridge)

let spirit_descriptor'_bridge =
  ([%scamltype: BrokerTypesHacks.spirit_descriptor] :> _ tzbridge)

let parse_wrapper_store str =
  let open Micheline in
  let parsed = Tzutils.Michelson.parse_expr str |> strip_tzresult_lwt in
  let root = root (parsed.expanded) in
  let p ?label x =
    let pf = match label with
      | Some label -> 
         Format.asprintf "[XX] %s : @[<hov 2>%a@]" label
      | None ->
         Format.asprintf "%a" in
    pf Tezos_client_007_PsDELPH1.Michelson_v1_printer.print_expr
      (strip_locations x) in
  let pair = Protocol.Michelson_v1_primitives.D_Pair in
  let left path = function
    | Prim (_, p,
      [l; _], _) when p = pair -> l
    | n -> failwith (p ~label:("left.unexpected "^path) n) in
  let right path = function
    | Prim (_, p,
      [_; r], _) when p = pair -> r
    | n -> failwith (p ~label:("right.unexpected "^path) n) in
  let l = left "l" root in
  let lr, rr =
    let r = right "r" root in
    left "lr" r, right "rr" r in
  let wstore = l
  and wfunc = lr
  and avatar_ident = rr
  in object
    method wstore = p wstore |> bytes_of_prefixed_hex
    method wfunc_node = wfunc
    method wfunc_unparsed = p wfunc
    method avatar_identity =
      p avatar_ident
      |> [%scamltype: BrokerTypesHacks.maybe_avatar_identity]#revert
      |> Option.get
  end

let pack_ccgen_bytes ccgen =
  let typ = [%scamltype.tz: TemplateTypes.ccgen] in
  Michelson.Handy.pack_data ~typ ccgen

let pack_ccgen_hex ccgen =
  pack_ccgen_bytes ccgen |> prefixed_hex_of_bytes

module DataModelConv = struct
  open Broker_models
  open BasicDatatypes

  let to_tz x = `mutez (ocmutez x)
  let to_bignat x = `nat (ocnat x)
  let to_key_hash x = `keyhash (ockh x)
  let to_address x = `addr (ocaddr x)
  let to_bytes x = `bytes (ocbytes x |> Bytes.to_string)

  let of_tz (`mutez x) = tzmutez x
  let of_bignat (`nat x) = tznat x
  let of_bytes (`bytes x) = tzbytes (Bytes.of_string x)
  let of_key_hash (`keyhash x) = SCaml.Key_hash x
  let of_address (`addr x) = tzaddr x

  let to_set func xs = `set (ocset xs |&> func)
  let of_set func (`set xs) = tzset (xs |&> func)
  let to_assoc keyfunc valfunc :
    ('a, 'b) SCaml.map -> ('c, 'd) assoc_map =
    fun es ->
    `assoc (ocmap es |&> keyfunc // valfunc)

  let bytes_to_address bytes =
    let open Tezos_protocol_007_PsDELPH1.Protocol.Alpha_context in
    Data_encoding.Binary.of_bytes_exn
      Contract.encoding (ocbytes bytes)
    |> Contract.to_b58check
    |> fun x ->  `addr x

  let bytes_to_key_hash bytes =
    let module Signature = Tezos_protocol_007_PsDELPH1.Protocol.Environment.Signature in
    Data_encoding.Binary.of_bytes_exn
      (Signature.Public_key_hash.encoding) (ocbytes bytes)
    |> Signature.Public_key_hash.to_b58check
    |> fun x ->  `keyhash x

  let to_fee_descriptor_data' :
        BrokerTypesHacks.fee_descriptor
        -> BrokerDataModels.fee_descriptor_data =
    function
    | ChargeFree -> `ChargeFree
    | ChargeOf {
        charge_amount;
        charge_collector;
      } ->
       `ChargeOf (
           `amount (charge_amount |> to_tz),
           `collector (charge_collector |> bytes_to_key_hash))

  let of_fee_descriptor_data :
        BrokerDataModels.fee_descriptor_data
        -> BrokerTypes.fee_descriptor =
    function
    | `ChargeFree -> ChargeFree
    | `ChargeOf (`amount amount, `collector collector) ->
       ChargeOf {
           charge_amount = amount |> of_tz;
           charge_collector = collector |> of_key_hash;
         }

  let to_broker_store_data :
        BrokerTypesHacks.broker_store
        -> BrokerDataModels.broker_store_data =
    fun {
      broker_version;
      broker_fee;
      broker_admins;
      broker_availability;
      broker_banner;
      templates;
      instances;
      instance_index;
      broker_revenue_collectable;
      } ->
    (`version (broker_version : string)),
    (`fee (broker_fee |> to_fee_descriptor_data')),
    (`admins (broker_admins |> to_set bytes_to_address : address set)),
    (`availability (broker_availability : bool)),
    (`banner (broker_banner : string)),
    (`templates (templates |> to_bignat // to_bignat : bignat*bignat)),
    (`instances (instances |> to_bignat // to_bignat : bignat*bignat)),
    (`instance_index (instance_index |> to_bignat : bignat )),
    (`broker_revenue_collectable
       (broker_revenue_collectable |> to_tz : tz))

  let to_template_descriptor_data :
        BrokerTypesHacks.template_descriptor
        -> BrokerDataModels.template_descriptor_data =
    fun {
      tmpldesc_ccgen (* : bytes *);
      tmpldesc_tmplhash (* : bytes *);

      tmpldesc_fee (* : fee_descriptor *);
      tmpldesc_availability (* : bool *);
      tmpldesc_bookhash (* : string option *);

      provider_revenue_collectable (* : tz *);
    } -> (
      (`ccgen (tmpldesc_ccgen |> to_bytes)),
      (`tmplhash (tmpldesc_tmplhash |> to_bytes)),
      (`fee (tmpldesc_fee |> to_fee_descriptor_data')),
      (`availability (tmpldesc_availability)),
      (`bookhash (tmpldesc_bookhash)),
      (`provider_revenue_collectable
         (provider_revenue_collectable |> to_tz)))

  let to_spirit_descriptor_data :
        BrokerTypesHacks.spirit_descriptor
        -> BrokerDataModels.spirit_descriptor_data =
    fun {
        sprtdesc_sprthash (* : string *);
        sprtdesc_requester (* : address *);
        sprtdesc_tmplid (* : nat *);

        sprtdesc_ensemble
        (* : (string, address) map; (\** [rclabel |-> address] * *);
      } ->
    
    ((`sprthash sprtdesc_sprthash  (* string *)),
     (`requester (sprtdesc_requester |> bytes_to_address)  (* address *)),
     (`tmplid (sprtdesc_tmplid |> to_bignat)  (* bignat *)),
     (`ensemble (sprtdesc_ensemble (* (string , address) assoc_map *)
                 |> to_assoc identity bytes_to_address))
     : BrokerDataModels.spirit_descriptor_data)

  let of_genesis_request_data :
        BrokerDataModels.genesis_request_data
        -> BrokerTypes.genesis_request =
    let open BrokerTypes in
    fun (
      `sprthash genreq_sprthash,
      `tmplid genreq_tmplid,
      `tmplhash genreq_tmplhash,
      `genparam genreq_genparam,
      `initbalance genreq_initbalance,
      `broker_fee genreq_broker_fee,
      `provider_fee genreq_provider_fee
    ) ->
    {
      genreq_sprthash;
      genreq_tmplid = genreq_tmplid |> of_bignat;
      genreq_tmplhash = genreq_tmplhash |> of_bytes;
      genreq_genparam = genreq_genparam |> of_bytes;
      genreq_initbalance = genreq_initbalance |> of_tz;

      genreq_broker_fee = genreq_broker_fee |> of_tz;
      genreq_provider_fee = genreq_provider_fee |> of_tz;
    }

  let of_broker_message_data :
        BrokerDataModels.broker_message_data
        -> BrokerTypes.broker_message =
    let open BrokerTypes in
    function
    | `TscaBrokerVersion -> TscaBrokerVersion

    | `GenesisRequest (`sudo sudo, `genreq genreq) ->
       let genreq = of_genesis_request_data genreq in
       GenesisRequest { sudo; genreq }

    | `ProviderRevenueCollection (
        `revenue_sources revenue_sources,
        `collector collector) ->
       ProviderRevenueCollection
         { revenue_sources = revenue_sources |> of_set (of_bignat);
           collector = collector |> of_key_hash;
         }

    | `BrokerRevenueCollection (`collector collector) ->
       BrokerRevenueCollection {
           collector = collector |> of_key_hash;
         }

    | `SudoCollectProviderRevenue (
        `revenue_sources revenue_sources,
        `collector collector,
        `reason reason) ->
       SudoCollectProviderRevenue {
           revenue_sources = revenue_sources |> of_set (of_bignat);
           collector = collector |> of_key_hash;
           reason
         }

    | `SudoCollectBrokerRevenue (`collector collector, `reason reason) ->
       SudoCollectBrokerRevenue {
           collector = collector |> of_key_hash;
           reason;
         }

    | `SudoUploadTemplate (
`tmplid tmplid, `ccgen ccgen, `tmplhash tmplhash) ->
       SudoUploadTemplate {
           tmplid = tmplid |> of_bignat;
           ccgen = ccgen |> of_bytes;
           tmplhash = tmplhash >? of_bytes;
         }


    | `SudoConfigTemplate
(`tmplid tmplid, `fee_updates fee_updates, `availability_updates availability_updates, `bookhash_updates bookhash_updates) ->
       SudoConfigTemplate {
           tmplid = tmplid |> of_bignat;
           fee_updates = fee_updates >? of_fee_descriptor_data;
           availability_updates; bookhash_updates;         }

    | `SudoAddAdmins xs ->
       SudoAddAdmins (xs |> of_set (of_address))
    | `SudoRemoveAdmins xs ->
       SudoRemoveAdmins (xs |> of_set (of_address))
    | `SudoConfigBroker
(`fee_updates fee_updates, `availability_updates availability_updates, `banner_updates banner_updates) ->
       SudoConfigBroker {
           fee_updates = fee_updates >? of_fee_descriptor_data;
           availability_updates; banner_updates;         }


end

