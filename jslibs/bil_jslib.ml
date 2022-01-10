open Jsutils
open Agency_apis.Bil_commons

let () =
  info "%s loading" __FILE__

let bil_info =
  let varident = "__tsca_bil_info" in
  jsvar_opt varident
  |> Option.map (fun js ->
         js |> yojson_of_js |> bil_info_of_yojson |>
           function
           | Ok x -> x
           | Error str -> failwith str                                   
       )

let init bil_info =
  let { sprthash; network; agency_base } = bil_info in
  let module Aii = Jsaii.Aii in
  let module AgencyTypes = Agency_apis.Aii_dir.AgencyTypes in
  let module Client = Jsaii.AiiClient in
  let open Jsaii.AiiJsBridge in
  let open Jsaii.PrivateHelpers in
  let module Proto0 = Aii.Proto0 in
  let agencyUrl path = agency_base^path in
  let js_sprthash = jsstr' sprthash in
  let rt func oc = oc |> func |> js_of_yojson in
  Js.export "TSCABookappInterface" (object%js
        val spiritStatus =
          let open Aii.IndexerLevel0 in
          (fun () -> sprthash) %>
          Client.invoke GetSpiritStatus
          %~> spirit_status_to_yojson
        val sprthash = js_sprthash
        val network = network |> AgencyTypes.tezos_network_to_yojson |> js_of_yojson
        val agencyUrl = ocstr' %> agencyUrl %> jsstr'
        val displaySpellAssistant = fun salabel targetIframe ->
          let salabel = ocstr' salabel in
          let url = agencyUrl ("/widgets/o/spellassistant/"^sprthash^"/"^salabel) in
          targetIframe##.src := (url |> jsstr')
        val explorerLinksForContract = AgencyTypes.(fun hash ->
          Client.invoke Proto0.ExplorerLinksForContract
            (network, ocstr' hash)
          >|= rt (List.map hyperlink_to_yojson %> (fun xs -> `List xs)))
        val explorerLinksForOperation = AgencyTypes.(fun hash ->
          Client.invoke Proto0.ExplorerLinksForOperation
            (network, ocstr' hash)
          >|= rt (List.map hyperlink_to_yojson %> (fun xs -> `List xs)))
        val interpretSpiritStatus = (fun silabel tmplversion ->
          let silabel = ocstr' silabel in
          let tmplversion =
            Js.(def tmplversion |> Optdef.to_option)
            |> Option.map ocstr' in
          Client.invoke Proto0.InterpretSpiritStatus {
              Proto0.sprthash; tmplversion; silabel
            }
          >|= (js_of_yojson % Proto0.spirit_interpretation_result_to_yojson)
        )
      end);
  info "%s loaded" __FILE__

let () =
  match bil_info with
  | None -> debug "%s failed to load: cannot determine bil_info" __FILE__
  | Some bil_info -> init bil_info
