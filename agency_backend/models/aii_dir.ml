open Api_directory

open Agency_types

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

    type spell_assistant_desc = {
        salabel : string;
        tmplversion : string;
        form_title : string;
        form_desc : string;
        form_major_button : string*string option; (* [title, desc] *)
        form_fields : Book_intf.TmplversionTypes.field_desc list;
      }
    [@@deriving sexp, yojson]

    type spell_assistant_input = {
        salabel : string;
        tmplversion : string;
        filled_form: (string (* field_name *) * string (* user_input *)) list;
      }
    [@@deriving sexp, yojson]

    type spell_assistant_interpretation =
      | UnsuccessfulSpellAssistantInterpretation of {
          error_message : string;
          hints : (string*string) list; (* (field_name |-> fix_hint) *)
        }
      | InvalidSpellAssistantInput of {
          error_message : string option;
        }
      | SuccessfulSpellAssistantInterpretation of (* interpreted spell *) string
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
      let spell_assistant_desc =
        sexp_of_spell_assistant_desc,
        spell_assistant_desc_of_sexp
      let spell_assistant_input =
        sexp_of_spell_assistant_input,
        spell_assistant_input_of_sexp
      let spell_assistant_interpretation =
        sexp_of_spell_assistant_interpretation,
        spell_assistant_interpretation_of_sexp
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
    | DescribeSpellAssistant :
        (string (* tmplversion *) * string (* salabel *),
         spell_assistant_desc) aii_api
    | SpellAssistantInterpret :
        (spell_assistant_input,
         spell_assistant_interpretation) aii_api

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
      intf#register_post
        DescribeSpellAssistant
        ~urlpath:"/api~003/proto0/describe-spell-assistant"
        ~name:"AII/Proto0/DescribeSpellAssistant"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.(tuple string string)
        ~respenc:Sexp_enc.spell_assistant_desc;
      intf#register_post
        SpellAssistantInterpret
        ~urlpath:"/api~003/proto0/spell-assistant-interpret"
        ~name:"AII/Proto0/SpellAssistantInterpret"
        ~desc:"(tbd - as name suggests)"
        ~reqenc:Sexp_enc.spell_assistant_input
        ~respenc:Sexp_enc.spell_assistant_interpretation;
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
