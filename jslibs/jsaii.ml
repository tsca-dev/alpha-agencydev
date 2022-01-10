open Jsutils

open Agency_apis.Aii_dir
open Agency_apis.Api_directory

open AgencyTypes

let api_endpoint =
  let default_endpoint = "localhost:8008" in
  let varident = "__tsca_agency_api_endpoint" in
  let endpoint = match jsvar_opt varident with
  | None ->
     default_endpoint |>
       Fn.tap (info "%s using default %s: %s" __FILE__ varident)
  | Some ep ->
     ep |> ocstr |>
       Fn.tap (info "%s using passed-in %s: %s" __FILE__ varident) in
  Auxtypes.Http_endpoint.parse_endpoint endpoint

module PrivateHelpers = struct
  let (>|=) = Promise.Infix.(>|=)
  let (%>) g f x = (f (g x))
  let (%|>) mg f x = mg x >|= f
  let (%~>) mg f = mg %|> (js_of_yojson % f)
  let (%~*>) mg f = mg %~> ((fun xs -> `List xs) % f)
  
  let (%?>) g f = g %> (
      function
      | Ok x -> x
      | Error str -> failwith str) %> f
  let (%~?>) g f = yojson_of_js %> g %?> f
end open PrivateHelpers

module Aii = AgencyInternalInterface
module AiiClient = ApiClient(Aii)(struct
               type 'x http_result = 'x promise
               let perform_request
                     ~url ~headers
                     ~meth ~body =
                 let body = match meth with
                   | `Get -> None
                   | _ -> Some body in
                 http_request ~headers ~meth ?body ~url
               let map_http_result f p = p >|= f
             end)
             (struct
               let endpoint = api_endpoint
             end)

module AiiJsBridge = struct

  module Client = AiiClient
  
  module RefMaster = struct
    open Aii.RefMaster
  
    let jsintf = object%js
        val defaultTezosNetwork = identity %>
          Client.invoke DefaultTezosNetwork
          %~> tezos_network_to_yojson
        val listAvailableTezosNetworks = identity %>
          Client.invoke ListAvailableTezosNetwork
          %~*> (List.map tezos_network_to_yojson)
        val listAdvertizedBooks = identity %>
          Client.invoke ListAdvertizedBooks
          %~*> (List.map advertized_book_to_yojson)
        val listBooksForTemplate = ocstr' %>
          Client.invoke ListBooksForTemplate
          %~*> (List.map bookhash_to_yojson)
        val getBookStatus = ocstr' %>
          Client.invoke GetBookStatus
          %~> book_status_to_yojson
        val getProviderInfo = ocstr' %>
          Client.invoke GetProviderInfo
          %~> provider_info_to_yojson
      end
  end
  
  module InfoBank = struct
    open Aii.InfoBank
  
    let jsintf = object%js
        val getBookEntry = ocstr' %>
          Client.invoke GetBookEntry
          %~> book_entry_to_yojson
      end
  end
  
  module IndexerLevel0 = struct
    open Aii.IndexerLevel0
  
    let jsintf = object%js
        val getSpiritInfo = ocstr' %>
          Client.invoke GetSpiritInfo
          %~> spirit_info_to_yojson
        val getSpiritStatus = ocstr' %>
          Client.invoke GetSpiritStatus
          %~> spirit_status_to_yojson
      end
  end
  
  module Proto0 = struct
    open Aii.Proto0

    let defaultClerkUserInfo =
      ref { name = "Bob Sakai";
            email = "b.sakai@example.org";
            tzaddr = "tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6"; }

    let jsintf =
      let tr func js = yojson_of_js js |> func |> Result.get_ok in
      let rt func oc = oc |> func |> js_of_yojson in
      object%js
        (* future work - store it in browser local storage *)
        val defaultClerkUserInfo = fun () ->
          !defaultClerkUserInfo
          |> user_info_to_yojson |> js_of_yojson
        val setDefaultClerkUserInfo =
          user_info_of_yojson %~?>
          fun userinfo -> defaultClerkUserInfo := userinfo
        val injectOperation =
          injection_request_of_yojson %~?>
          Client.invoke InjectOperation
          %~> injection_result_to_yojson
        val bookAppUrlForSpirit = ocstr' %>
          Client.invoke BookAppUrlForSpirit
          %~> hyperlink_to_yojson
        val explorerLinksForContract = fun nw hash ->
          Client.invoke ExplorerLinksForContract (
              tr tezos_network_of_yojson nw,
              ocstr' hash)
          >|= rt (List.map hyperlink_to_yojson %> (fun xs -> `List xs))
        val explorerLinksForOperation = fun nw hash ->
          Client.invoke ExplorerLinksForOperation (
              tr tezos_network_of_yojson nw,
              ocstr' hash)
          >|= rt (List.map hyperlink_to_yojson %> (fun xs -> `List xs))
        val processGenesisRequest =
          genesis_request_of_yojson %~?>
          Client.invoke ProcessGenesisRequest
          %~> genesis_processed_to_yojson
        val processInvocationRequest =
          invocation_request_of_yojson %~?>
          Client.invoke ProcessInvocationRequest
          %~> invocation_processed_to_yojson
        val simulateGenesis = fun req proc ->
          Client.invoke SimulateGenesis (
              req |> tr genesis_request_of_yojson,
              proc |> tr genesis_processed_of_yojson)
          >|= rt simulation_result_to_yojson
        val simulateInvocation = fun req proc ->
          Client.invoke SimulateInvocation (
              req |> tr invocation_request_of_yojson,
              proc |> tr invocation_processed_of_yojson)
          >|= rt simulation_result_to_yojson
        val interpretSpiritStatus =
          spirit_interpretation_request_of_yojson %~?>
          Client.invoke InterpretSpiritStatus
          %~> spirit_interpretation_result_to_yojson
      end
  end
  
  module TezosUtils = struct
    open Aii.TezosUtils
  
    let jsintf = object%js
        val calculateaddressfromledgerpublickey = ocstr' %>
          Client.invoke CalculateAddressFromLedgerPublicKey
          %|> jsstr'
      end
  end

end
