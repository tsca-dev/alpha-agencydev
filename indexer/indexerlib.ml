open Commons

module Basetypes = struct
  open Sexplib.Std

  type mutez = int64
  [@@deriving sexp]

  type hexbytes = bytes

  module Hexbytes_sexpconv = struct
    type t = string
    [@@deriving sexp]

    let sexp_of_hexbytes bytes =
      hex_of_bytes bytes
      |> sexp_of_t
    let hexbytes_of_sexp sexp =
      t_of_sexp sexp |> bytes_of_hex
  end

  let sexp_of_hexbytes, hexbytes_of_sexp =
    Hexbytes_sexpconv.(sexp_of_hexbytes, hexbytes_of_sexp)
end

module IndexedTypes = struct
  open Basetypes
  open Sexplib.Std

  type mutez = int64
  [@@deriving sexp]

  type fee_descriptor =
    | ChargeFree
    | ChargeOf of {
        charge_amount : mutez;
        charge_collector : string;
      }
  [@@deriving sexp]

  type broker_entry = {
      idxbrk_address : string;
      idxbrk_version : string;

      idxbrk_availability : bool;
      idxbrk_fee : fee_descriptor;
      idxbrk_banner : string;
      idxbrk_admins : string list;
      idxbrk_revenue_collectable : mutez;

      idxbrk_template_bigmapid : int;
      idxbrk_template_count : int;

      idxbrk_spirit_bigmapid : int;
      idxbrk_spirit_count : int;
    }
  [@@deriving sexp]

  type template_entry = {
      idxtmpl_broker : string;
      idxtmpl_tmplid : int;
      idxtmpl_tmplhash : string;
      idxtmpl_availability : bool;
      idxtmpl_fee : fee_descriptor;
      idxtmpl_bookhash : string option;
      idxtmpl_revenue_collectable : mutez;
    }
  [@@deriving sexp]

  type spirit_entry = {
      idxsprt_broker : string;
      idxsprt_sprthash : string;
      idxsprt_requester : string;
      idxsprt_tmplid   : int;
      idxsprt_tmplhash : string;
      idxsprt_ensemble : (string*string) list; (** [rclabel |-> address] *)
    }
  [@@deriving sexp]

  type avatar_entry = {
      idxav_address : string;
      idxav_broker : string;
      idxav_sprthash : string;
      idxav_rclabel : string;
      idxav_balance : mutez;
      idxav_wstore : hexbytes;   (** wstore, michelson bytes *)
      idxav_wstore_unpacked : string option;  (** wstore, unpacked *)
    }
  [@@deriving sexp]

  type block_info = {
      idxbl_chain_id : string;
      idxbl_blockhash : string;
      idxbl_level : int32;
      idxbl_timestamp : int64; (** unix timestamp in second resolution *)
    }
  [@@deriving sexp]

  module Conv = struct
    open Brokerlib

    let of_fee_descriptor = function
    | BrokerTypes.ChargeFree -> ChargeFree
    | BrokerTypes.ChargeOf {
        charge_amount;
        charge_collector;
      } ->
       ChargeOf {
            charge_amount = ocmutez charge_amount;
            charge_collector = ockh charge_collector;
          }

    let of_broker_store' ~broker
          BrokerTypesHacks.{ broker_version;
            broker_fee;
            broker_admins;
            broker_availability;
            broker_banner;
            templates; instances;
            broker_revenue_collectable;
            _; } =
    let of_fee_descriptor = function
      | BrokerTypesHacks.ChargeFree -> ChargeFree
      | BrokerTypesHacks.ChargeOf {
          charge_amount;
          charge_collector;
         } ->
         ChargeOf {
             charge_amount = ocmutez charge_amount;
             charge_collector = ockh charge_collector;
           } in
    let bigmapid (id,_) = ocnat id in
    let bigmapcount (_,count) = ocnat count in
    { idxbrk_address = broker;
      idxbrk_version = broker_version;

      idxbrk_availability = broker_availability;
      idxbrk_fee = broker_fee |> of_fee_descriptor;
      idxbrk_banner = broker_banner;
      idxbrk_admins = broker_admins |> ocset |&> ocaddr;
      idxbrk_revenue_collectable = broker_revenue_collectable |> ocmutez;

      idxbrk_template_bigmapid = bigmapid templates;
      idxbrk_template_count = bigmapcount templates;

      idxbrk_spirit_bigmapid = bigmapid instances;
      idxbrk_spirit_count = bigmapcount instances; }

    open BrokerTypes

    let of_template_descriptor ~broker ~tmplid
          { tmpldesc_tmplhash;
            tmpldesc_fee;
            tmpldesc_availability;
            tmpldesc_bookhash;
            provider_revenue_collectable;
            _; } = {
        idxtmpl_broker = broker;
        idxtmpl_tmplid = tmplid;
        idxtmpl_tmplhash =
          tmpldesc_tmplhash |> ocbytes
          |> Tzcrypto.tmplhash_of_hashbytes ~check_length:false;
        idxtmpl_availability = tmpldesc_availability;
        idxtmpl_fee = of_fee_descriptor tmpldesc_fee;
        idxtmpl_bookhash = tmpldesc_bookhash;
        idxtmpl_revenue_collectable = ocmutez provider_revenue_collectable;
      }

    let of_spirit_descriptor ~broker ~tmplhash_lookup
          { sprtdesc_sprthash;
            sprtdesc_requester;
            sprtdesc_tmplid;
            sprtdesc_ensemble;
          } = {
        idxsprt_broker = broker;
        idxsprt_sprthash = sprtdesc_sprthash;
        idxsprt_requester = ocaddr sprtdesc_requester;
        idxsprt_tmplid = ocnat sprtdesc_tmplid;
        idxsprt_tmplhash =
          tmplhash_lookup (ocnat sprtdesc_tmplid);
        idxsprt_ensemble = (
          ocmap sprtdesc_ensemble
          |&> fun (rclabel, addr) -> rclabel, ocaddr addr
        );
      }
  end
end

module IndexV0 = struct
  open IndexedTypes

  type t = {
      block_info : block_info;

      brokers : broker_entry array; (** indexed by [brkxid] *)
      templates : template_entry array; (** indexed by [tmplxid] *)
      spirits : spirit_entry array; (** indexed by [sprtxid] *)
      avatars : avatar_entry array; (** indexed by [avxid] *)

      brkindex : (string, int) Hashtbl.t;
      (** [ktaddr |-> brkxid] *)
      tmplindex : (string, int) Hashtbl.t;
      (** [tmplhash |-> [tmplxid ..]], multi entry per key *)
      sprtindex : (string, int) Hashtbl.t;
      (** [sprthash |-> sprtxid] *)
      avindex : (string, int) Hashtbl.t;
      (** [ktaddr |-> avxid] *)
    }

  module Serialization = struct
    let magic_000 = "tsca-broker-index:000"
    let current_magic = magic_000

    type container = {
        magic : string;
        index : t;
      }

    let marshaling_flags = Marshal.[Compat_32]

    let pack_index : t -> container = fun index ->
      { magic = current_magic;
        index; }

    let to_bytes : t -> bytes = fun index ->
      let container = pack_index index in
      Marshal.(to_bytes container marshaling_flags)

    let deserialize : ('src -> container) -> 'src -> t option = fun read src ->
      try
        let { magic; index } = read src in
        if magic <> current_magic
        then None
        else Some index
      with _ -> None

    let from_bytes : bytes -> t option =
      deserialize Marshal.(Fn.flip from_bytes 0)

    let from_channel : in_channel -> t option =
      deserialize Marshal.from_channel

  end

  let block_info index = index.block_info

  let list_brokers
        ?availability_filter
        index =
    let filter entry =
      let by_availability = match availability_filter with
        | None -> true
        | Some v -> entry.idxbrk_availability = v in
      by_availability in
    index.brokers
    |> Array.to_seq
    |> Seq.filter filter
    |> List.of_seq

  let list_templates
        ?broker_filter
        ?availability_filter
        ?tmplhash_filter
        index =
    let filter entry =
      let by_broker = match broker_filter with
        | None -> true
        | Some brokers -> List.mem entry.idxtmpl_broker brokers in
      let by_availability = match availability_filter with
        | None -> true
        | Some v -> entry.idxtmpl_availability = v in
      let by_tmplhash = match tmplhash_filter with
        | None -> true
        | Some h -> entry.idxtmpl_tmplhash = h in
      by_broker && by_availability && by_tmplhash in
    index.templates
    |> Array.to_seq
    |> Seq.filter filter
    |> List.of_seq
  
  let lookup_broker index ~broker =
    Hashtbl.find_opt index.brkindex broker
    |> Option.map (fun brkxid ->
         index.brokers.(brkxid))

  let lookup_spirit index ~sprthash =
    Hashtbl.find_opt index.sprtindex sprthash
    |> Option.map (fun sprtxid ->
         index.spirits.(sprtxid))

  let lookup_template index ~tmplhash =
    Hashtbl.find_all index.tmplindex tmplhash
    |> List.map (fun tmplxid -> index.templates.(tmplxid))

  let lookup_avatar' index ~ktaddr =
    let (>>=) = Option.bind in
    Hashtbl.find_opt index.avindex ktaddr >>= fun avxid ->
    Some index.avatars.(avxid)

  let lookup_avatar index ~sprthash ~rclabel =
    let (>>=) = Option.bind in
    lookup_spirit index ~sprthash >>= fun spirit ->
    List.assoc_opt rclabel spirit.idxsprt_ensemble >>= fun ktaddr ->
    Hashtbl.find_opt index.avindex ktaddr >>= fun avxid ->
    Some index.avatars.(avxid)

  let lookup_spirit_with_avatars index ~sprthash =
    let open MonadOps(Option) in
    lookup_spirit index ~sprthash >>= fun spirit ->
    spirit.idxsprt_ensemble |&> (fun (rclabel, ktaddr) ->
      lookup_avatar' index ~ktaddr >>= fun avatar ->
      Some (rclabel, avatar)
    ) >>=* fun avatars ->
    Some (spirit, avatars)

end
