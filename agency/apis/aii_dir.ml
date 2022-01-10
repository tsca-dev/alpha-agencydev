open Api_directory

module AgencyTypes = struct
  open Sexplib.Std

  type sprthash = string [@@deriving sexp, yojson]
  type tmplhash = string [@@deriving sexp, yojson]
  type bookhash = string [@@deriving sexp, yojson]
  type providerident = string [@@deriving sexp, yojson]
  type hexbytes = string [@@deriving sexp, yojson]
  type tzaddr = string [@@deriving sexp, yojson]
  type rclabel = string [@@deriving sexp, yojson]

  type mutez = Int64.t [@@deriving yojson]
  let sexp_of_mutez x =
    [%sexp_of: string list]
      ["mutez"; Int64.to_string x]
  let mutez_of_sexp sexp =
    match [%of_sexp: string list] sexp with
    | ["mutez"; x] -> Int64.of_string x
    | _ -> invalid_arg Format.(
        asprintf "unparsable as mutez: %a"
          Sexplib.Sexp.pp_hum sexp)

  type hyperlink = {
      title : string;
      url : string;
      synopsis : string option;
    }
  [@@deriving sexp, yojson]

  type tezos_network = {
      netident : string; (** e.g. "carthagenet" *)
      chain_id : string; (** e.g. "NetXyJVJ3mkBox6" *)
    }
  [@@deriving sexp, yojson]

  type advertized_book = {
      bookident : string;
      bookhash : string;
      title : string;
      synopsis : string;
    }
  [@@deriving sexp, yojson]

  type book_charges = {
      agency_charge : mutez;
      provider_charge : mutez;
    }
  [@@deriving sexp, yojson]

  type book_review_results = {
      contract_complexity : string;
      certification_status : string;
    }
  [@@deriving sexp, yojson]

  type provider_info = {
      providerident : string;
      display_name : string;
      introduction : string;
      website : string;
      contact : string;
    }
  [@@deriving sexp, yojson]

  type book_status = {
      bookident : string;
      bookhash : string;
      charges : book_charges;
      review_results : book_review_results;
    }
  [@@deriving sexp, yojson]

  type book_entry = {
      bookident : string;
      bookhash : string;
      title : string;
      synopsis : string;
      tmplhash : string;
      provider : string; (* providerident *)
      contract_parameters_en :
        (string*string) list; (* [param, description] *)
      contract_terms_en :
        string list;
      contract_caveats_en :
        string list;
      specifications :
        hyperlink list;
    }
  [@@deriving sexp, yojson]

  type spirit_info = {
      sprthash : string;
      tmplhash : string;
      network : tezos_network;
      broker : tzaddr;
      requester : tzaddr;
      ensemble : (string*tzaddr) list;
    }
  [@@deriving sexp, yojson]

  module Sexp_enc = struct
    let list (xs,sx) = sexp_of_list xs, list_of_sexp sx
    let tuple (xs1,sx1) (xs2,sx2) =
      let lenc, ldec = list (identity, identity) in
      (fun (o1, o2) -> lenc [xs1 o1; xs2 o2]),
      ((function
        | [s1; s2] -> (sx1 s1, sx2 s2)
        | ss -> invalid_arg Format.(
            asprintf "unparsable as tuple: %a"
              Sexplib.Sexp.pp_hum (lenc ss)) )
       % ldec)

    let mutez = sexp_of_mutez, mutez_of_sexp
    let hyperlink = sexp_of_hyperlink, hyperlink_of_sexp
    let string = sexp_of_string, string_of_sexp
    let tezos_network =
      sexp_of_tezos_network,
      tezos_network_of_sexp
    let advertized_book =
      sexp_of_advertized_book,
      advertized_book_of_sexp
    let provider_info =
      sexp_of_provider_info,
      provider_info_of_sexp
    let book_status =
      sexp_of_book_status,
      book_status_of_sexp
    let book_entry =
      sexp_of_book_entry,
      book_entry_of_sexp
    let spirit_info =
      sexp_of_spirit_info,
      spirit_info_of_sexp
    let string_list = list string
  end
end open AgencyTypes

module AgencyInternalInterface = struct
  type (_, _) aii_api = ..

  open ApiRegistarType(struct
       type ('reqty, 'respty) typed_invp =
         ('reqty, 'respty) aii_api
     end)

  open Sexplib.Std

  module RefMaster = struct
    type (_, _) aii_api +=
    | DefaultTezosNetwork :
        (unit, tezos_network) aii_api
    | ListAvailableTezosNetwork :
        (unit, tezos_network list) aii_api
    | ListAdvertizedBooks :
        (unit, advertized_book list) aii_api
    | ListBooksForTemplate :
        (tmplhash, bookhash list) aii_api
    | GetBookStatus :
        (bookhash, book_status) aii_api
    | GetProviderInfo :
        (providerident, provider_info) aii_api
    
    let register_all (intf : api_registrar) =
      intf#register_get
        DefaultTezosNetwork
        ~urlpath:"/api~003/refmaster/default-tezos-network"
        ~name:"AII/RefMaster/DefaultTezosNetwork"
        ~desc:"(tbd - as name suggests)"
        ~respenc:Sexp_enc.tezos_network;
      intf#register_get
        ListAvailableTezosNetwork
        ~urlpath:"/api~003/refmaster/list-available-tezos-networks"
        ~name:"AII/RefMaster/ListAvailableTezosNetwork"
        ~desc:"(tbd - as name suggests)"
        ~respenc:Sexp_enc.(list tezos_network);
      intf#register_get
        ListAdvertizedBooks
        ~urlpath:"/api~003/refmaster/list-advertized-books"
        ~name:"AII/RefMaster/ListAdvertizedBooks"
        ~desc:"(tbd - as name suggests)"
        ~respenc:Sexp_enc.(list advertized_book);
      intf#register_post
        ListBooksForTemplate
        ~urlpath:"/api~003/refmaster/list-books-for-template"
        ~name:"AII/RefMaster/ListBooksForTemplate"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.string
        ~respenc:Sexp_enc.(list string);
      intf#register_post
        GetBookStatus
        ~urlpath:"/api~003/refmaster/get-book-status"
        ~name:"AII/RefMaster/GetBookStatus"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.string
        ~respenc:Sexp_enc.book_status;
      intf#register_post
        GetProviderInfo
        ~urlpath:"/api~003/refmaster/get-provider-info"
        ~name:"AII/RefMaster/GetProviderInfo"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.string
        ~respenc:Sexp_enc.provider_info;
  end

  module InfoBank = struct
    type (_, _) aii_api +=
    | GetBookEntry :
        (bookhash, book_entry) aii_api

    let register_all (intf : api_registrar) =
      intf#register_post
        GetBookEntry
        ~urlpath:"/api~003/infobank/get-book-entry"
        ~name:"AII/InfoBank/GetBookEntry"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.string
        ~respenc:Sexp_enc.book_entry
  end

  module IndexerLevel0 = struct

    type avatar_status = {
        balance : mutez;
        wstore : hexbytes;
        wstore_unpacked : string option;
      }
    [@@deriving sexp, yojson]

    type spirit_status = {
        spirit : spirit_info;
        avatars : (rclabel*avatar_status) list;
      }
    [@@deriving sexp, yojson]

    type (_, _) aii_api +=
    | GetSpiritInfo :
        (sprthash, spirit_info) aii_api
    | GetSpiritStatus :
        (sprthash, spirit_status) aii_api

    module Sexp_enc = struct
      include Sexp_enc

      let spirit_status =
        sexp_of_spirit_status,
        spirit_status_of_sexp
    end

    let register_all (intf : api_registrar) =
      intf#register_post
        GetSpiritInfo
        ~urlpath:"/api~003/indexerl0/get-spirit-info"
        ~name:"AII/IndexerL0/GetSpiritInfo"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.string
        ~respenc:Sexp_enc.spirit_info;
      intf#register_post
        GetSpiritStatus
        ~urlpath:"/api~003/indexerl0/get-spirit-status"
        ~name:"AII/IndexerL0/GetSpiritStatus"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.string
        ~respenc:Sexp_enc.spirit_status;
  end

  module Proto0 = struct
    type user_info = {
        name   : string;
        email  : string;
        tzaddr : tzaddr;
      }
    [@@deriving sexp, yojson]

    type instruction_line =
      | InstructionComment of string
      | InstructionParameter of string*string (* [param, desc] *)
      | TezosClientCommand of (bool*string (* [param?, contents]*)) list
    [@@deriving sexp, yojson]

    type genesis_request = {
        network : tezos_network;
        template : tmplhash;
        requester : tzaddr;
        name  : string;
        email : string;
        spell : string;
      }
    [@@deriving sexp, yojson]

    type genesis_processed = {
        sprthash : string;
        initbal  : mutez;
        agency_charge : mutez;
        provider_charge : mutez;
        cli_instructions : instruction_line list;
        broker : tzaddr;
        tmplid : int;
        genparam : hexbytes;

        txn_argument : string;
        txn_amount : mutez;
      }
    [@@deriving sexp, yojson]

    type invocation_request = {
        network : tezos_network;
        spirit : sprthash;
        requester : tzaddr;
        name  : string;
        email : string;
        spell : string;
      }
    [@@deriving sexp, yojson]

    type invocation_processed = {
        sprthash : string;
        invparam : string;
        invparam_unpacked : string option;
        amount : mutez;
        dest_rclabel : string;
        dest_ktaddr : tzaddr;
        cli_instructions : instruction_line list;

        txn_argument : string;
        txn_amount : mutez;
      }
    [@@deriving sexp, yojson]

    type simulation_result =
      | SimulationSucceeded of {
          unsigned_transaction : hexbytes;
          watermark : hexbytes;
          simulation_output : string;
        }
      | SimulationFailed of {
          error_message : string;
        }
    [@@deriving sexp, yojson]

    type injection_request = {
        network : tezos_network;
        unsigned_transaction : hexbytes;
        signature : hexbytes;
      }
    [@@deriving sexp, yojson]

    type injection_result =
      | InjectionSucceeded of {
          ophash : string;
          originated_contracts : tzaddr list;
        }
      | InjectionFailed of {
          error_message : string;
        }
    [@@deriving sexp, yojson]

    type spirit_interpretation_request = {
        sprthash : string;
        tmplversion : string option;
        silabel : string;
      }
    [@@deriving sexp, yojson]

    type spirit_interpretation_result =
      | InterpretationResult of string
      | InterpretationError of {
          message : string
        }
      | NoSuchSpiritInterpreter
      | TmplversionNotAvailable
    [@@deriving sexp, yojson]

    module Sexp_enc = struct
      include Sexp_enc

      let genesis_request =
        sexp_of_genesis_request,
        genesis_request_of_sexp
      let genesis_processed =
        sexp_of_genesis_processed,
        genesis_processed_of_sexp
      let invocation_request =
        sexp_of_invocation_request,
        invocation_request_of_sexp
      let invocation_processed =
        sexp_of_invocation_processed,
        invocation_processed_of_sexp
      let injection_request =
        sexp_of_injection_request,
        injection_request_of_sexp
      let injection_result =
        sexp_of_injection_result,
        injection_result_of_sexp
      let simulation_result =
        sexp_of_simulation_result,
        simulation_result_of_sexp
      let spirit_interpretation_request =
        sexp_of_spirit_interpretation_request,
        spirit_interpretation_request_of_sexp
      let spirit_interpretation_result =
        sexp_of_spirit_interpretation_result,
        spirit_interpretation_result_of_sexp
    end    

    type (_, _) aii_api +=
    | InjectOperation :
        (injection_request, injection_result) aii_api
    | BookAppUrlForSpirit :
        (sprthash, hyperlink) aii_api
    | ExplorerLinksForContract :
        (tezos_network*string (* ktaddr *), hyperlink list) aii_api
    | ExplorerLinksForOperation :
        (tezos_network*string (* ophash *), hyperlink list) aii_api
    | ProcessGenesisRequest :
        (genesis_request, genesis_processed) aii_api
    | ProcessInvocationRequest :
        (invocation_request, invocation_processed) aii_api
    | SimulateGenesis :
        (genesis_request*genesis_processed,
         simulation_result) aii_api
    | SimulateInvocation :
        (invocation_request*invocation_processed,
         simulation_result) aii_api
    | InterpretSpiritStatus :
        (spirit_interpretation_request,
         spirit_interpretation_result) aii_api

    let register_all (intf : api_registrar) =
      intf#register_post
        InjectOperation
        ~urlpath:"/api~003/proto0/inject-operation"
        ~name:"AII/Proto0/InjectOperation"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.injection_request
        ~respenc:Sexp_enc.injection_result;
      intf#register_post
        BookAppUrlForSpirit
        ~urlpath:"/api~003/proto0/bookapp-url-for-spirit"
        ~name:"AII/Proto0/BookAppUrlForSpirit"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.string
        ~respenc:Sexp_enc.hyperlink;
      intf#register_post
        ExplorerLinksForContract
        ~urlpath:"/api~003/proto0/explorer-links-for-contract"
        ~name:"AII/Proto0/ExplorerLinksForContract"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.(tuple tezos_network string)
        ~respenc:Sexp_enc.(list hyperlink);
      intf#register_post
        ExplorerLinksForOperation
        ~urlpath:"/api~003/proto0/explorer-links-for-operation"
        ~name:"AII/Proto0/ExplorerLinksForOperation"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.(tuple tezos_network string)
        ~respenc:Sexp_enc.(list hyperlink);
      intf#register_post
        ProcessGenesisRequest
        ~urlpath:"/api~003/proto0/process-genesis-request"
        ~name:"AII/Proto0/ProcessGenesisRequest"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.genesis_request
        ~respenc:Sexp_enc.genesis_processed;
      intf#register_post
        ProcessInvocationRequest
        ~urlpath:"/api~003/proto0/process-invocation-request"
        ~name:"AII/Proto0/ProcessInvocationRequest"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.invocation_request
        ~respenc:Sexp_enc.invocation_processed;
      intf#register_post
        SimulateGenesis
        ~urlpath:"/api~003/proto0/simulate-genesis"
        ~name:"AII/Proto0/SimulateGenesis"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.(tuple genesis_request genesis_processed)
        ~respenc:Sexp_enc.simulation_result;
      intf#register_post
        SimulateInvocation
        ~urlpath:"/api~003/proto0/simulate-invocation"
        ~name:"AII/Proto0/SimulateInvocation"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.(tuple invocation_request invocation_processed)
        ~respenc:Sexp_enc.simulation_result;
      intf#register_post
        InterpretSpiritStatus
        ~urlpath:"/api~003/proto0/interpret-spirit-status"
        ~name:"AII/Proto0/InterpretSpiritStatus"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.spirit_interpretation_request
        ~respenc:Sexp_enc.spirit_interpretation_result;
  end

  module TezosUtils = struct
    type (_, _) aii_api +=
    | CalculateAddressFromLedgerPublicKey :
        (hexbytes, tzaddr) aii_api

    let register_all (intf : api_registrar) =
      intf#register_post
        CalculateAddressFromLedgerPublicKey
        ~urlpath:"/api~003/tezosutils/calculate-address-from-ledger-public-key"
        ~name:"AII/TezosUtils/CalculateAddressFromLedgerPublicKey"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.string
        ~respenc:Sexp_enc.string;
  end

  let register_all intf =
    RefMaster.register_all intf;
    InfoBank.register_all intf;
    IndexerLevel0.register_all intf;
    Proto0.register_all intf;
    TezosUtils.register_all intf;

  include SimpleApiDirectory(struct
       type nonrec api_registrar = api_registrar
       type ('reqty, 'respty) typed_invp =
         ('reqty, 'respty) aii_api
       let register_all = register_all
     end)
end
