open Brokerlib

let parse_initst_opts() =
  let admin, banner = ArgOptions.(
      get_option_exn (StringOption "-admin"),
      get_option_exn (StringOption "-banner")) in
  { admin; banner }


let trbytes bytes = SCaml.Bytes (Commons.hex_of_bytes bytes)

let parse_charge =
  let open BrokerTypes in
  function
  | "free" -> ChargeFree
  | str ->
     Scanf.sscanf str "%g>%s" (fun tez collector ->
         ChargeOf {
             charge_amount = tztez tez;
             charge_collector = SCaml.(Key_hash collector);
           }
       )

let _ =
  let open ArgOptions in
  if has_flag "-initst-michelson" then (
    let initst = broker_init_store (parse_initst_opts()) in
    [%scamltype: BrokerTypes.broker_store]#convert initst |> print_endline
  ) else if has_flag "-initst-type" then (
    [%scamltype.tz: BrokerTypes.broker_store] |> print_endline
  ) else if has_flag "-tzcli-broker-originate" then (
    if has_flag "-help" then (
      print_endline "broker.exe -tzcli-broker-originate # must be followed by those arguments/options";
      print_endline "    -admin <tzaddr> -banner <string>  # init storage arguments";
      print_endline "    [-tzprog <progname>=$tezos_client] # name/path of the tezos-client executable";
      print_endline "    [-ktname <name>=tsca_broker]   # name of the originating contract";
      print_endline "    [-src <wallet account>=$tzsrc] # originator of the contract";
      print_endline "    [--burn-cap <tz>=18]           # additional options passed to tezos-client";
      print_endline "    [--force] [--dry-run]          # additional flags passed to tezos-client";
      print_endline "    [-tmp-ktfile <path>=/tmp/kt.tz]   # temp file to store the contract code";
      exit 2
    );
    let tzprog = get_option_d (StringOption "-tzprog") "$tezos_client"
    and ktname = get_option_d (StringOption "-ktname") "tsca_broker"
    and force, dryrun = has_flag "--force", has_flag "--dry-run"
    and source = get_option_d (StringOption "-src") "$tzsrc"
    and burn_cap = get_option_d (StringOption "--burn-cap") "18"
    and ktpath =
      get_option_d (StringOption "-tmp-ktfile") "/tmp/kt.tz" in
    let initst =
      let initst = broker_init_store (parse_initst_opts()) in
      [%scamltype: BrokerTypes.broker_store]#convert initst in
    let write_file path str =
      let ch = open_out path in output_string ch str; close_out ch in
    [%scamlcontract BrokerContract] |> write_file ktpath;
    Format.eprintf "INFO: contract code written to %s@." ktpath;
    Format.printf "%s %s originate contract %s transferring 0 from %s \
                   running %s --init '%s' --burn-cap %s -q\
                   %s%s%s" tzprog (get_option_d (StringOption "-globalargs") "")
      ktname source ktpath initst burn_cap
      (if force then " --force" else "")
      (if dryrun then " --dry-run" else "")
      (get_absolute_args() |&> (fun s -> " "^s) |> String.concat "")
  ) else if has_flag "-broker-contract" then (
    [%scamlcontract BrokerContract] |> print_endline
  ) else if has_flag "-pack-ccgen" then (
    let ccgen_path = get_option_exn (StringOption "-pack-ccgen") in
    slurp_file ccgen_path |> pack_ccgen_hex |> print_endline
  ) else if has_flag "-hash-ccgen" then (
    let ccgen_path = get_option_exn (StringOption "-hash-ccgen") in
    let open Digestif.SHA256 in
    let raw =
      slurp_file ccgen_path |> pack_ccgen_bytes
      |> digest_bytes |> to_raw_string in
    let bytes = Bytes.of_string raw in
    let chopped = Bytes.sub bytes 0 16 in
    let b58checked =
      chopped |> Bytes.to_string
      |> Tzcrypto.(b58check_encode Arbitrarity.tmplhash_scheme) in
    let open Commons in
    Format.printf "rawhash: %s@." (hex_of_bytes bytes);
    Format.printf "chopped rawhash: %s@." (hex_of_bytes chopped);
    Format.printf "b58checked: %s@." b58checked;
  ) else if has_flag "-tzcli-sudo-upload-template" then (
    if has_flag "-help" then (
      print_endline "broker.exe -tzcli-sudo-upload-template # must be followed by those arguments/options";
      print_endline "    -tmplid <nat> -ccgen <path>  # init storage arguments";
      print_endline "    [-tmplhash <0xhex>]            # sha256 hash of the packed ccgen";
      print_endline "    [-tzprog <progname>=$tezos_client]  # name/path of the tezos-client executable";
      print_endline "    [-kt <wallet contract>=tsca_broker] # name/address of the broker contract";
      print_endline "    [-src <wallet account>=$tzsrc]      # source of the operation";
      print_endline "    [--burn-cap <tz>=18]           # additional options passed to tezos-client";
      print_endline "    [--force] [--dry-run]          # additional flags passed to tezos-client";
      print_endline "    [-tmp-ktfile <path>=/tmp/kt.tz]     # temp file to store the contract code";
      print_endline "";
      print_endline "hint : $tezos_client hash data <0xhex> of type bytes";
      exit 2
    );
    let ccgen_path = get_option_exn (StringOption "-ccgen") in
    let tmplid = SCaml.Nat (get_option_exn (IntOption "-tmplid")) in
    let tmplhash = get_option (StringOption "-tmplhash")
                   |> Option.map Tzutils.bytes_of_prefixed_hex in
    let ccgen = slurp_file ccgen_path |> pack_ccgen_bytes in
    let ccgen, tmplhash = trbytes ccgen, Option.map trbytes tmplhash in
    let msg = BrokerTypes.SudoUploadTemplate {
        tmplid; ccgen; tmplhash
      } in
    let arg = message_bridge#convert msg in
    let tzprog = get_option_d (StringOption "-tzprog") "$tezos_client"
    and kt = get_option_d (StringOption "-kt") "tsca_broker"
    and force, dryrun = has_flag "--force", has_flag "--dry-run"
    and source = get_option_d (StringOption "-src") "$tzsrc"
    and burn_cap = get_option_d (StringOption "--burn-cap") "18" in
    Format.printf "%s %s transfer 0 from %s to %s \
                   --arg '%s' --burn-cap %s -q\
                   %s%s%s" tzprog (get_option_d (StringOption "-globalargs") "")
      source kt arg burn_cap
      (if force then " --force" else "")
      (if dryrun then " --dry-run" else "")
      (get_absolute_args() |&> (fun s -> " "^s) |> String.concat "")
  ) else if has_flag "-tzcli-sudo-demand-genesis" then (
    if has_flag "-help" then (
      print_endline "broker.exe -tzcli-sudo-demand-genesis # docs todo";
      exit 2
    );
    let genreq_sprthash = get_option_exn (StringOption "-sprthash") in
    let genreq_tmplid = SCaml.Nat (get_option_exn (IntOption "-tmplid")) in
    let genreq_tmplhash = get_option_exn (StringOption "-tmplhash") |> Tzutils.bytes_of_prefixed_hex |> trbytes in
    let genreq_genparam = get_option_exn (StringOption "-genparam") |> Tzutils.bytes_of_prefixed_hex |> trbytes in
    let genreq_initbalance = SCaml.Tz (get_option_exn (FloatOption "-initbal")) in
    let genreq_broker_fee, genreq_provider_fee = tztez0, tztez0 in
    let arg = BrokerTypes.GenesisRequest {
        sudo = true;
        genreq = {
            genreq_sprthash;
            genreq_tmplid;
            genreq_tmplhash;
            genreq_genparam;
            genreq_initbalance;
            genreq_broker_fee;
            genreq_provider_fee;
          };
        } |> message_bridge#convert in
    let tzprog = get_option_d (StringOption "-tzprog") "$tezos_client"
    and kt = get_option_d (StringOption "-kt") "tsca_broker"
    and force, dryrun = has_flag "--force", has_flag "--dry-run"
    and source = get_option_d (StringOption "-src") "$tzsrc"
    and burn_cap = get_option_d (StringOption "--burn-cap") "18" in
    let amount = SCaml.(
        match genreq_broker_fee +$ genreq_provider_fee +$ genreq_initbalance with
        | Tz x -> x) in
    Format.printf "%s %s transfer %g from %s to %s \
                   --arg '%s' --burn-cap %s -q\
                   %s%s%s" tzprog (get_option_d (StringOption "-globalargs") "")
      amount source kt arg burn_cap
      (if force then " --force" else "")
      (if dryrun then " --dry-run" else "")
      (get_absolute_args() |&> (fun s -> " "^s) |> String.concat "")
  ) else if has_flag "-tzcli-sudo-config-template" then (
    let tmplid = get_option_exn (IntOption "-tmplid") in
    let availability_updates =
      if has_flag "-disable" then (Some false)
      else if has_flag "-enable" then (Some true)
      else None in
    let fee_updates =
      get_option (StringOption "-fee") |> Option.map parse_charge in
    let arg = BrokerTypes.SudoConfigTemplate {
        tmplid = tznat tmplid;
        fee_updates;
        availability_updates;
        bookhash_updates = None;
        } |> message_bridge#convert in
    let tzprog = get_option_d (StringOption "-tzprog") "$tezos_client"
    and kt = get_option_d (StringOption "-kt") "tsca_broker"
    and force, dryrun = has_flag "--force", has_flag "--dry-run"
    and source = get_option_d (StringOption "-src") "$tzsrc"
    and burn_cap = get_option_d (StringOption "--burn-cap") "18" in
    let amount = 0. in
    Format.printf "%s %s transfer %g from %s to %s \
                   --arg '%s' --burn-cap %s -q\
                   %s%s%s" tzprog (get_option_d (StringOption "-globalargs") "")
      amount source kt arg burn_cap
      (if force then " --force" else "")
      (if dryrun then " --dry-run" else "")
      (get_absolute_args() |&> (fun s -> " "^s) |> String.concat "")
  ) else if has_flag "-tzcli-sudo-config-broker" then (
    let availability_updates =
      if has_flag "-disable" then (Some false)
      else if has_flag "-enable" then (Some true)
      else None in
    let fee_updates =
      get_option (StringOption "-fee") |> Option.map parse_charge in
    let arg = BrokerTypes.SudoConfigBroker {
        fee_updates;
        availability_updates;
        banner_updates = None;
        } |> message_bridge#convert in
    let tzprog = get_option_d (StringOption "-tzprog") "$tezos_client"
    and kt = get_option_d (StringOption "-kt") "tsca_broker"
    and force, dryrun = has_flag "--force", has_flag "--dry-run"
    and source = get_option_d (StringOption "-src") "$tzsrc"
    and burn_cap = get_option_d (StringOption "--burn-cap") "18" in
    let amount = 0. in
    Format.printf "%s %s transfer %g from %s to %s \
                   --arg '%s' --burn-cap %s -q\
                   %s%s%s" tzprog (get_option_d (StringOption "-globalargs") "")
      amount source kt arg burn_cap
      (if force then " --force" else "")
      (if dryrun then " --dry-run" else "")
      (get_absolute_args() |&> (fun s -> " "^s) |> String.concat "")
  ) else if has_flag "-tell-next-tmplid" then (
    let input = slurp_stdin() in
    match [%scamltype: BrokerTypesHacks.broker_store]#revert input with
    | None -> failwith "unable to parse input, which is expected to be the broker storage in Michelson literal"
    | Some st ->
       let BrokerTypesHacks.{ templates = (_, SCaml.(Nat nxtmplid)); _ } = st in
       print_endline (string_of_int nxtmplid)
  ) else if has_flag "-inj-sudo-demand-genesis" then (
    if has_flag "-help" then (
      print_endline "broker.exe -inj-sudo-demand-genesis # docs todo";
      exit 2
    );
    let genreq_sprthash = get_option_exn (StringOption "-sprthash") in
    let genreq_tmplid = SCaml.Nat (get_option_exn (IntOption "-tmplid")) in
    let genreq_tmplhash = get_option_exn (StringOption "-tmplhash") |> Tzutils.bytes_of_prefixed_hex |> trbytes in
    let genreq_genparam = get_option_exn (StringOption "-genparam") |> Tzutils.bytes_of_prefixed_hex |> trbytes in
    let genreq_initbalance = SCaml.Tz (get_option_exn (FloatOption "-initbal")) in
    let genreq_broker_fee, genreq_provider_fee = tztez0, tztez0 in
    let arg = BrokerTypes.GenesisRequest {
        sudo = true;
        genreq = {
            genreq_sprthash;
            genreq_tmplid;
            genreq_tmplhash;
            genreq_genparam;
            genreq_initbalance;
            genreq_broker_fee;
            genreq_provider_fee;
          };
        } |> message_bridge#convert in
    let open Tznode in
    (* let open Tezos_base__TzPervasives in *)
    let open Tezos_protocol_007_PsDELPH1.Protocol.Alpha_context in
    let burn_cap = get_option_d (StringOption "--burn-cap") "18"
      |> Tez.of_string |> Stdlib.Option.get in
    let logger =
      if has_flag "-l" then `FullStderr else `Null in
    let dry_run = has_flag "-dry" in
    let base_dir = get_option (StringOption "-base-dir") in
    let endpoint = get_option (StringOption "-endpoint") in
    let chain, block =
      let open Bcutils in
      get_option (StringOption "-chain") |> Option.fold ~none:`Main ~some:chain_of_string,
      get_option (StringOption "-block") |> Option.fold ~none:(`Head 0) ~some:block_of_string in
    let source, destination =
      get_option_exn (StringOption "-src"),
      get_option_exn (StringOption "-kt") in
    let amount = SCaml.(
        match genreq_broker_fee +$ genreq_provider_fee +$ genreq_initbalance with
        | Tz x -> x) in
    let amount = Tez.of_string Format.(sprintf "%g" amount) |> Stdlib.Option.get in
    let cctxt =
      if has_flag "-mockup"
      then mockup_client_context ?base_dir ()
      else client_context ?base_dir ?endpoint ~logger () in
    let open Tzutils in
    if has_flag "-simulate" then (
      match [@warning "-27"] Tznode.simulate
              ~verbose_signing:true
              cctxt ~source ~destination ~amount ~arg ~burn_cap ~chain ~block ()
            |> strip_lwt with
      | `Failed errmsg ->
         print_endline "Simulate failed, see reason below.";
         print_endline errmsg
      | `Succeeded (logs, (watermark, bytes), contracts, op, result) ->
         print_endline Format.(sprintf "Simulate succeeded. %d contract would be originated"
                                 (List.length contracts));
         print_endline ("pre-signed op : "^(hex_of_bytes bytes));
         print_endline ("    watermark : "^(hex_of_bytes watermark));
         print_endline "log entries:";
         flush_all();
         logs |!> (fun (ch, message) ->
           Format.(printf "[%s] %s@." ch message)
         )
    ) else (
      Tznode.transfer cctxt ~source ~destination ~amount ~arg ~burn_cap ~chain ~block ~dry_run ()
      |> strip_tzresult_lwt
    )
  ) else if has_flag "-inj-operation" then (
    let open Tznode in
    let logger =
      if has_flag "-l" then `FullStderr else `Null in
    let base_dir = get_option (StringOption "-base-dir") in
    let endpoint = get_option (StringOption "-endpoint") in
    let chain, block =
      let open Bcutils in
      get_option (StringOption "-chain") |> Option.fold ~none:`Main ~some:chain_of_string,
      get_option (StringOption "-block") |> Option.fold ~none:(`Head 0) ~some:block_of_string in
    let cctxt =
      if has_flag "-mockup"
      then mockup_client_context ?base_dir ()
      else client_context ?base_dir ?endpoint ~logger () in
    let open Tzutils in
    let unsigned = get_option_exn (StringOption "-opbytes") |> bytes_of_hex in
    let signature = `Base58check (get_option_exn (StringOption "-sig")) in
    inject cctxt ~chain ~block ~unsigned ~signature
    |> strip_tzresult_lwt |> fst
    |> print_endline
  ) else if has_flag "-dummy-wfunc" then (
    [%scamlvalue TemplateTypes.dummy_wfunc] |> print_endline
  ) else if has_flag "-foo" then (
    let open Bcutils in
    let open Tznode in
    (* let open Tezos_base__TzPervasives in *)
    let open Tezos_protocol_007_PsDELPH1.Protocol.Alpha_context in
    let logger =
      if has_flag "-l" then `FullStderr else `Null in
    let dry_run = has_flag "-dry" in
    let endpoint = get_option (StringOption "-endpoint") in
    let chain, block =
      get_option (StringOption "-chain") |> Option.fold ~none:`Main ~some:chain_of_string,
      get_option (StringOption "-block") |> Option.fold ~none:(`Head 0) ~some:block_of_string
      in
    let source, destination =
      get_option_exn (StringOption "-src"),
      get_option_exn (StringOption "-dest") in
    let arg =
      get_option (StringOption "-arg") in
    let amount = get_option_exn (StringOption "-amount") |> Tez.of_string |> Stdlib.Option.get in
    let cctxt = client_context ?endpoint ~logger () in
    transfer cctxt ~source ~destination ~amount ?arg ~chain ~block ~dry_run ()
    |> Tzutils.strip_tzresult_lwt
  ) else (
    print_endline "usage for broker.exe made by the tsca team in 2020";
    print_endline "  -broker-contract           # print the broker contract code to stdout";
    print_endline "  -initst-michelson ..       # print the michelson literal for a broker contract init storage";
    print_endline "  -initst-michelson ..       # print the michelson type for the broker contract's storage";
    print_endline "  -pack-ccgen ..             # type check and pack a ccgen program";
    print_endline "  -hash-ccgen ..             # calculate the tmplhash for a ccgen";
    print_endline "  -tzcli-broker-originate .. # print cmd to originate a fresh broker contract";
    print_endline "  -tzcli-sudo-upload-template .. # print cmd to upload a template";
    print_endline "  -tzcli-sudo-config-broker ..   # tbd";
    print_endline "  -tzcli-sudo-demand-genesis ..  # tbd";
    print_endline "  -inj-sudo-demand-genesis ..  # tbd";
    print_endline "  -tell-next-tmplid ..       # parse storage contents and tell next tmplid";
  )

