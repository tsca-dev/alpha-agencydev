open Book_intf
open BookTypes
open TmplversionTypes
open Common

type book = BookTypes.t

let book_of_crowdfunding : book = {
    entry = {
      bookident = "crowdfunding";
      bookhash = "bkBookOfCrowdfundingProto0";
      title = "Book of Crowdfunding";
      synopsis = "Book of Crowdfunding is a simple vesting contract";
      tmplhash = "tmpL1Q76bCk4nhT2Wf4e2K5rjyubW7gR2v";
      (* based on tsca-formaldev/a68d9132:crowdfunding.ccgen *)
      provider = tsca_team.providerident;
      contract_parameters_en = [
          "raisers", "the raisers of this crowdfunding, who could collect the fund after a funding campaign";
          "funding_start", "the starting timestamp of the funding period";
          "funding_end", "the ending timestamp of the funding period";
          "unconditional_refund_start", "if the fund is not collected by the raisers before this timestamp, \
                                         contributors would be able to claim refunds";
        ];
      contract_terms_en = [
          "contributors could make contributions within the funding period starting from [funding_start] and ending at [funding_end]";
          "When making a contribution, the contributor must provide a valid [refund_address]. \
           Each [refund_address] must be an implicit account.";
          "After the funding period, anyone listed in the [raisers] could request a transfer of the raised fund \
           before the [unconditional_refund_start]. \
           Only one such transfer could be requested and the transfer will be made in whole.";
          "If no raisers’ transfer were requested and the [unconditional_refund_start] had been passed, \
           each contributor could request a refund of their contribution from his/her [refund_address]. \
           Each such refund will be made once in whole.";
          "No parameter of the contract could be amended after the origination of the contract. \
           Nor could the [refund_address] of a contribution be amended."
        ];
      contract_caveats_en = [
          "Once originated, there is no way a crowdfunding could be stopped or cancelled.";
          "Once a contribution is made, the contribution cannot be withdrawn. \
           (Refund will be available if the raised fund is not accessed by the raisers before \
           [unconditional_refund_start].)";
          "It is the responsibility of each contributor to ensure that the [refund_address] \
           provided with a contribution remains valid when a refund is needed.";
          "Access to the raised fund, or the refund cannot be made in parts, \
           i.e. such transactions will be made in whole amounts at once.";
        ];
      specifications = [
          { synopsis = Some "Effective template contents in the TSCA Ccgen format";
            title = "Effective Template";
            url = "https://github.com/kxcteam/tsca-formaldev/blob/master/crowdfunding.ccg" };
        ]
    };
    agency_charge = tez 0.5;
    provider_charge = tez 1.5;
    contract_complexity = "Medium-High";
    certification_status = "Formally verified with Coq";
  }

(* type decl:

  (pair
     (pair (set (* %raisers *) address) (map (* %refund_table *) address mutez))
     (pair (pair (bool (* %withdrawn *)) (timestamp (* %funding_start *)))
           (pair (timestamp (* %funding_end *)) (timestamp (* %unconditional_refund_start *))))).

 *)

type contract_storage_model = {
    raisers : string list;              (* tz: (set address) *)
    refund_table : (string*int64) list; (* tz: (map address mutez) *)
    withdrawn : bool;                   (* tz: bool *)
    funding_start : int64;              (* tz: timestamp *)
    funding_end : int64;                (* tz: timestamp *)
    unconditional_refund_start : int64; (* tz: timestamp *)
  } [@@deriving yojson]

type invparam_model =
  | Contribute of {
      refund_address : string; (* tz: key_hash *)
    }
  | Withdraw of {
      beneficiary : string; (* tz: address *)
    }
  | Refund of {
      eligible_address : string; (* tz: address *)
    }

module StorageConv (Sdk : TemplateVersionSdk) = struct
  open Sdk.Michelson_data

  let michelson_of_storage_model =
    fun { raisers; refund_table; withdrawn;
          funding_start; funding_end;
          unconditional_refund_start } ->
    let p01 =
      of_pair
        ((raisers (* : (set address) *)
          |&> of_address |> of_set) ,
         (refund_table (* : (map address mutez) *)
          |&> of_address // of_mutez |> of_map)) in
    let p021 =
      of_pair ((of_bool withdrawn), (of_timestamp funding_start)) in
    let p022 =
      of_pair ((of_timestamp funding_end), (of_timestamp unconditional_refund_start)) in
    let p02 = of_pair (p021, p022) in
    of_pair (p01, p02)

  let michelson_to_storage_model m =
    let p01, p02 = to_pair m in
    let raisers, refund_table = to_pair p01 in
    let p021, p022 = to_pair p02 in
    let withdrawn, funding_start = to_pair p021 in
    let funding_end, unconditional_refund_start = to_pair p022 in
    {
      raisers = raisers |> to_set |&> to_address;
      refund_table = refund_table |> to_map |&> to_address // to_mutez;
      withdrawn = withdrawn |> to_bool;
      funding_start = funding_start |> to_timestamp;
      funding_end = funding_end |> to_timestamp;
      unconditional_refund_start = unconditional_refund_start |> to_timestamp;
    }

  let michelson_of_invparam_model = function
    | Contribute { refund_address } ->
       of_or (`Left (of_key_hash refund_address))
    | Withdraw { beneficiary } ->
       of_or (`Right (of_or (`Left (of_address beneficiary))))
    | Refund { eligible_address } ->
       of_or (`Right (of_or (`Right (of_address eligible_address))))

end


let crowdfunding_version001 : tmplversion =
  let open Sexplib in
  let open MonadOps(ResultWithErrmsg) in
  let error fmt = Format.kasprintf (fun msg -> Result.error msg) fmt in
  let pp_sexp = Sexp.pp_mach in
  let parse_sexp str =
    try Sexp.of_string str |> pure
    with Failure errmsg -> Result.error errmsg in
  let parse_tzaddr = function
    | Sexp.(Atom addr) -> pure addr
    | _ -> Result.error "malformed spell" in
  let parse_key_hash = function
    | Sexp.(Atom kh) -> pure kh
    | _ -> Result.error "malformed spell" in
  let parse_tez_as_mutez str = match String.chop_suffix "tz" str with
    | None -> Result.error ("mulformed spell: "^str^" is not valid amount in tez")
    | Some x -> Q.of_string x |> Q.(mul (of_int 1_000_000)) |> Q.to_int64 |> pure in
  let parse_iso8601 str =
    match Ptime.of_rfc3339 str with
    | Ok (x, _, _) -> Ptime.(x |> to_float_s |> Int64.of_float) |> pure
    | Error _ -> error "malformed spell: %s is not valid iso8601 timestamp" str in
  let to_iso8601 t =
    Ptime.(of_float_s (Int64.to_float t)
           |> Option.get
           |> to_rfc3339 ~tz_offset_s:0) in
  let parse_genspell = function
    (* expected form: '(crowdfunding0.gen
                         (tz1.. ...)
                         2000-00-00T00:00:00Z
                         2000-00-00T00:00:00Z
                         2000-00-00T00:00:00Z)' *)
    | Sexp.(List [
        (Atom "crowdfunding0.gen");
        (List raisers_sexp);
        (Atom funding_start_lit);
        (Atom funding_end_lit);
        (Atom unconditional_refund_start_lit);
      ]) ->
       raisers_sexp |&> parse_tzaddr >>=* fun raisers ->
       parse_iso8601 funding_start_lit >>= fun funding_start ->
       parse_iso8601 funding_end_lit >>= fun funding_end ->
       parse_iso8601 unconditional_refund_start_lit >>= fun unconditional_refund_start ->
       pure (raisers, funding_start, funding_end, unconditional_refund_start)
    | x -> error "malformed genesis spell: %a" pp_sexp x in
  let parse_invspell = function
    (* expected form: one of
       - '(crowdfunding0.contribute 0.0tz tz1..)'
       - '(crowdfunding0.withdraw tz1..)'
       - '(crowdfunding0.refund tz1..)'  *)
    | Sexp.(List [
        (Atom "crowdfunding0.contribute");
        (Atom amount_lit);
        (Atom refund_address_lit);
      ]) ->
       parse_tez_as_mutez amount_lit >>= fun amount ->
       parse_key_hash (Atom refund_address_lit) >>= fun refund_address ->
       pure (amount, Contribute {refund_address})
    | Sexp.(List [
        (Atom "crowdfunding0.withdraw");
        (Atom beneficiary_lit);
      ]) ->
       parse_tzaddr (Atom beneficiary_lit) >>= fun beneficiary ->
       pure (0L, Withdraw { beneficiary })
    | Sexp.(List [
        (Atom "crowdfunding0.refund");
        (Atom eligible_address_lit);
      ]) ->
       parse_tzaddr (Atom eligible_address_lit) >>= fun eligible_address ->
       pure (0L, Refund { eligible_address })
    | x -> error "malformed invocation spell: %a" pp_sexp x in
  let genspell_interpreter (module Sdk : TemplateVersionSdk) ~spell =
    (parse_sexp spell >>= parse_genspell >>= fun
       (raisers,
        funding_start, funding_end,
        unconditional_refund_start) ->
     let init_storage = {
         raisers;
         refund_table = [];
         withdrawn = false;
         funding_start;
         funding_end;
         unconditional_refund_start } in
     let module Conv = StorageConv(Sdk) in
     Conv.michelson_of_storage_model init_storage |> pure)
    |> Result.fold ~ok:Lwt.Infix.(fun (genparam) ->
      Sdk.pack_data genparam >>= fun packed -> pure (packed, tez 0.) |> Lwt.return)
         ~error:(Lwt.return % Result.error)
  in
  let invspell_interpreter (module Sdk : TemplateVersionSdk) ~spell =
    (parse_sexp spell >>= parse_invspell >>= fun (amount, param) ->
     let module Conv = StorageConv(Sdk) in
     (amount, Conv.michelson_of_invparam_model param) |> pure)
    |> Result.fold ~ok:Lwt.Infix.(fun (amount, param) -> Sdk.pack_data param >>= fun packed ->
       Lwt.return (pure ("main", packed, amount))) ~error:(Lwt.return % Result.error)
  in
  let spell_assistants = [

      (* Genesis Spell Assistant *)
      begin
        let fn_raisers = "Fund Raisers" in
        let fn_funding_start = "Funding Period Start" in
        let fn_funding_end = "Funding Period End" in
        let fn_ucr_start = "Unconditional Refund Start" in
        { salabel = "genesis.basic01";
          form_title = "Crowdfunding Contract Genesis Parameters";
          form_desc = "Enter the genesis parameters for a Crowdfunding contract";
          form_major_button = "Launch", None;
          form_fields = [
              { name = fn_raisers; desc = "raisers of the funding campaign";
                typ = `list (`tzaddr, ","); mandated = true;
                placeholder = None;
                doc = Some "the raisers of this crowdfunding, who could collect the fund after a funding campaign";
                requirement_desc = Some "minimum one raiser";
              };
              { name = fn_funding_start; desc = "the start of funding period";
                typ = `atom `timestamp; mandated = true;
                placeholder = None;
                doc = Some "the starting timestamp of the funding period";
                requirement_desc = Some "must be in the future but within 90 days";
              };
              { name = fn_funding_end; desc = "the end of funding period";
                typ = `atom `timestamp; mandated = true;
                placeholder = None;
                doc = Some "the ending timestamp of the funding period";
                requirement_desc = Some ("must be after but within 270 days of " ^ fn_funding_start);
              };
              { name = fn_ucr_start; desc = "the start of unconditional refunding period";
                typ = `atom `timestamp; mandated = true;
                placeholder = None;
                doc = Some "if the fund is not collected by the raisers before this timestamp, \
                            contributors would be able to claim refunds";
                requirement_desc = Some ("must be after but within 90 days of " ^ fn_funding_end);
              };
            ];
          form_interpreter = (fun (module Sdk) fvs ->
            let raisers =
              List.assoc fn_raisers fvs
              |> String.split_on_char ','
              |?> (Fn.negate String.empty_trimmed) in
            let funding_start = List.assoc fn_funding_start fvs in
            let funding_end = List.assoc fn_funding_end fvs in
            let funding_ucr_start = List.assoc fn_ucr_start fvs in
            match raisers with
            | [] -> Error ("There must be at least one raiser.", [
                          fn_raisers, "minimum one raiser"
                      ])
            | _ ->
               sprintf "(crowdfunding0.gen (%s) %s %s %s)"
                 (raisers |> String.concat " ")
                 funding_start funding_end
                 funding_ucr_start
               |> Result.ok)
        }
      end;

      (* Contribute Spell Assistant *)
      begin
        let fn_amount = "Amount" in
        let fn_refund_address = "Refunding Address" in
        { salabel = "contribute.basic01";
          form_title = "Crowdfunding Contract Contribution Parameters";
          form_desc = "Enter the parameters for give a contribution to a Crowdfunding contract";
          form_major_button = "Contribute", None;
          form_fields = [
              { name = fn_amount; desc = "contributing amount";
                typ = `atom `amount; mandated = true;
                placeholder = Some "0";
                doc = Some "the amount of tez to be contributed to the crowdfunding campaign";
                requirement_desc = None;
              };
              { name = fn_refund_address; desc = "refunding address";
                typ = `atom `tzaddr; mandated = true;
                placeholder = None;
                doc = Some "to which the refund would be sent to in case the raisers did not withdraw the raised fund";
                requirement_desc = None;
              };
            ];
          form_interpreter = (fun (module Sdk) fvs ->
            let amount = List.assoc fn_amount fvs in
            let refund_address = List.assoc fn_refund_address fvs in
            sprintf "(crowdfunding0.contribute %stz %s)" amount refund_address |> Result.ok)
        }
      end;

      (* Withdraw Spell Assistant *)
      begin
        let fn_beneficiary = "Beneficiary Address" in
        { salabel = "withdraw.basic01";
          form_title = "Crowdfunding Contract Withdrawal Parameters";
          form_desc = "Enter the parameters for withdrawing the raised fund from a finished Crowdfunding contract";
          form_major_button = "Withdraw", None;
          form_fields = [
              { name = fn_beneficiary; desc = "beneficiary address";
                typ = `atom `tzaddr; mandated = true;
                placeholder = None;
                doc = Some "to which the raised fund would be sent to";
                requirement_desc = None;
              };
            ];
          form_interpreter = (fun (module Sdk) fvs ->
            let beneficiary = List.assoc fn_beneficiary fvs in
            sprintf "(crowdfunding0.withdraw %s)" beneficiary |> Result.ok)
        }
      end;

      (* Refund Request Spell Assistant *)
      begin
        let fn_refundee = "Refundee Address" in
        { salabel = "refund.basic01";
          form_title = "Crowdfunding Contract Refund Request Parameters";
          form_desc = "Enter the parameters for requesting a refund from a finished but not abandoned Crowdfunding contract";
          form_major_button = "Refund", None;
          form_fields = [
              { name = fn_refundee; desc = "refundee address";
                typ = `atom `tzaddr; mandated = true;
                placeholder = None;
                doc = Some "to which the refund would be sent to";
                requirement_desc = None;
              };
            ];
          form_interpreter = (fun (module Sdk) fvs ->
            let refundee = List.assoc fn_refundee fvs in
            sprintf "(crowdfunding0.refund %s)" refundee |> Result.ok)
        }
      end;

    ] in
  let open struct
        type status = [
          | `before_funding
          | `funding_period
          | `funding_finished_fund_available
          | `funding_finished_fund_withdrawn
          | `refunding | `refunded
          ]
        let status_to_yojson = (
            function
          | `before_funding -> "before_funding"
          | `funding_period -> "funding_period"
          | `funding_finished_fund_available -> "funding_finished_fund_available"
          | `funding_finished_fund_withdrawn -> "funding_finished_fund_withdrawn"
          | `refunding -> "refunding"
          | `refunded -> "refunded"
          ) &> (fun x -> `String x)
        type interpreted_basic = {
            contract : string;
            balance : string;
            raisers : string list;
            status : status;
            funding_start : string; (* iso8601 *)
            funding_end : string; (* iso8601 *)
            unconditional_refund_start : string; (* iso8601 *)
            refund_table : ((* eligible_address *) string*mutez) list;
          } [@@deriving to_yojson]
    end in
  let spirit_interpreter_1 = {
      silabel = "storage.json";
      interpreter = (fun (module Sdk : TemplateVersionSdk) spirit ->
        let open Lwt.Infix in
        let main = spirit.avatars |> List.find (fun av -> av.rclabel = "main") in
        Sdk.unpack_data main.wstore >>= fun store ->
        let module Conv = StorageConv(Sdk) in
        let json =
          store |> Conv.michelson_to_storage_model
          |> contract_storage_model_to_yojson
          |> Yojson.Safe.to_basic in
        Sdk.unparse_json json |> pure |> Lwt.return)
    } in
  let spirit_interpreter_2 = {
      silabel = "basic.json";
      interpreter = (fun (module Sdk : TemplateVersionSdk) spirit ->
        let open Lwt.Infix in
        let main = spirit.avatars |> List.find (fun av -> av.rclabel = "main") in
        Sdk.unpack_data main.wstore >>= fun store ->
        let module Conv = StorageConv(Sdk) in
        let { raisers; refund_table; withdrawn;
          funding_start; funding_end;
          unconditional_refund_start } =
          store |> Conv.michelson_to_storage_model in
        Sdk.now() >>= fun now ->
        let status : status =
          if now < funding_start then `before_funding
          else if now < funding_end then `funding_period
          else if funding_end < now && now < unconditional_refund_start then (
            if withdrawn then `funding_finished_fund_withdrawn
            else `funding_finished_fund_available
          ) else if withdrawn then `funding_finished_fund_withdrawn
          else (if (main.balance = 0L) then `refunded else `refunding) in
        let interpretation = {
            contract = main.address;
            balance = Q.(div (of_int64 main.balance)
                           (of_int 1_000_000)
                         |> to_float)
                      |> Format.sprintf "%g";
            raisers;
            status;
            funding_start = funding_start |> to_iso8601;
            funding_end = funding_end |> to_iso8601;
            unconditional_refund_start = unconditional_refund_start |> to_iso8601;
            refund_table;
          } in
        let json = interpretation |> interpreted_basic_to_yojson |> Yojson.Safe.to_basic in
        Sdk.unparse_json json |> pure |> Lwt.return)
    } in
  { tmplversion = "tpvCrowdfundingProto001";
    tmplhash = book_of_crowdfunding.entry.tmplhash;
    genspell_interpreter;
    invspell_interpreter;
    spell_assistants;
    spirit_interpreters = [spirit_interpreter_1; spirit_interpreter_2]; }

module Crowdfunding = struct
  let book_entry = book_of_crowdfunding
  let tmplhash = book_entry.entry.tmplhash
  let tmplversion_001 = crowdfunding_version001
end
