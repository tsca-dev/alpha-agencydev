open Sexplib.Std

module BasicDatatypes = struct
  type bignat = [ `nat of int ] [@@deriving sexp]
  type tz = [ `mutez of int64 ] [@@deriving sexp]
  type bytes = [ `bytes of string ] [@@deriving sexp]
  type address = [ `addr of string ] [@@deriving sexp]
  type key_hash = [ `keyhash of string ] [@@deriving sexp]

  type ('k, 'v) assoc_map = [ `assoc of ('k*'v) list ] [@@deriving sexp]
  type 't set = [ `set of 't list ] [@@deriving sexp]
end open BasicDatatypes

module TemplateDataModels = struct

  type wrapper_store_data =
    [`storage of bytes]
    *[`identity of avatar_identity_data option]
  [@@deriving sexp]

  and avatar_identity_data =
    [`broker of address]
    *[`sprthash of string]
    *[`rclabel of string]
  [@@deriving sexp]

end

module BrokerDataModels = struct

  type fee_descriptor_data = [
    | `ChargeFree
    | `ChargeOf of
        [`amount of tz]
        *[`collector of key_hash]
    ]
  [@@deriving sexp]

  type template_descriptor_data =
    [`ccgen of bytes]
    *[`tmplhash of bytes]
    *[`fee of fee_descriptor_data]
    *[`availability of bool]
    *[`bookhash of string option]
    *[`provider_revenue_collectable of tz]
  [@@deriving sexp]

  type genesis_request_data =
    [`sprthash of string]
    *[`tmplid of bignat]
    *[`tmplhash of bytes]
    *[`genparam of bytes]
    *[`initbalance of tz]
    *[`broker_fee of tz]
    *[`provider_fee of tz]
  [@@deriving sexp]

  type spirit_descriptor_data =
    [`sprthash of string]
    *[`requester of address]
    *[`tmplid of bignat]
    *[`ensemble of (string, address) assoc_map]
  [@@deriving sexp]

  type broker_store_data =
    [`version of string]
    *[`fee of fee_descriptor_data]
    *[`admins of address set]
    *[`availability of bool]
    *[`banner of string]
    *[`templates of bignat*bignat]
    *[`instances of bignat*bignat]
    *[`instance_index of bignat ]
    *[`broker_revenue_collectable of tz]
  [@@deriving sexp]

  type broker_message_data = [
    | `TscaBrokerVersion

    | `GenesisRequest of
        [`sudo of bool]
        *[`genreq of genesis_request_data]

    | `ProviderRevenueCollection of
        [`revenue_sources of bignat set
         (* set of [tmplid] *) ]
        *[`collector of key_hash
          (* beneficiary of the withdraw transfer *) ]

    | `BrokerRevenueCollection of
        [`collector of key_hash
        (* beneficiary of the withdraw transfer *) ]

    | `SudoCollectProviderRevenue of
        [`revenue_sources of bignat set
         (* set of [tmplid] *)]
        *[`collector of key_hash
          (* beneficiary of the withdraw transfer *) ]
        *[`reason of string option
          (* arbitrary string for informative purposes *) ]

    | `SudoCollectBrokerRevenue of
        [`collector of key_hash
         (* beneficiary of the withdraw transfer *)]
        *[`reason of string option
          (* arbitrary string for informative purposes *)]

    | `SudoUploadTemplate of
        [`tmplid of bignat]
        *[`ccgen of bytes]
        *[`tmplhash of bytes option
          (* if provided, will be checked against [sha256 ccgen] *)]

    | `SudoConfigTemplate of
        [`tmplid of bignat]
        *[`fee_updates          of fee_descriptor_data option]
        *[`availability_updates of bool option]
        *[`bookhash_updates     of string option option]

    | `SudoAddAdmins of address set
    | `SudoRemoveAdmins of address set
    | `SudoConfigBroker of
        [`fee_updates          of fee_descriptor_data option]
        *[`availability_updates of bool option]
        *[`banner_updates       of string option]
    ]
  [@@deriving sexp]

end
