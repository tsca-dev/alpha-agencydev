open Book_intf
open BookTypes
open TmplversionTypes
open Common

type book = BookTypes.t

let book_of_frozen : book = {
    entry = {
      bookident = "frozen";
      bookhash = "bkBookOfFrozenProto0";
      title = "Book of Frozen";
      synopsis = "Book of Frozen is a simple vesting contract";
      tmplhash = "tmpL1Q6jgUaZeMvDpuw5axKCwYXGUSvX2P";
      (* based on tsca-formaldev/10f6b10:frozen.ccgen *)
      provider = tsca_team.providerident;
      contract_parameters_en = [
          "fund_amount", "the total amount in tez to be locked in this contract";
          "frozen_until", "the unlocking date and time after which funds could be withdrawn";
          "fund_owners", "fund owners who could withdraw after frozen_until";
        ];
      contract_terms_en = [
          "no additional fund could be added to the contract";
          "only invocations issued by one fo the accounts listed in the fund_owners \
           could be used to withdraw from the fund";
          "the fund could only be withdrawn at or after the timestamp frozen_until";
          "the contract parameters will remain unchange once originated"
        ];
      contract_caveats_en = [
          "there is absolutely no way to withdraw the fund without the access to \
           one of the accounts isted in the fund_owners parameter";
          "there is absolutely no way to withdraw the fund before the frozen_until \
           timestamp"
        ];
      specifications = [
          { synopsis = Some "Equivalent standalone Michelson contract";
            title = "Equivalent Michelson Contract";
            url = "https://github.com/kxcteam/tsca-formaldev/blob/master/frozen.tz" };
          { synopsis = Some "Effective template contents in the TSCA Ccgen format";
            title = "Effective Template";
            url = "https://github.com/kxcteam/tsca-formaldev/blob/master/frozen.ccg" };
          { synopsis = Some "Technical notes on the implementation and verification";
            title = "Technical Notes";
            url = "https://github.com/kxcteam/tsca-formaldev/blob/master/tn-frozen.pdf" };
          { synopsis = Some "Mechanized verification script in Mi-cho-coq";
            title = "Formal Verification Script";
            url = "https://github.com/kxcteam/tsca-formaldev/blob/master/frozen.v" };
        ]
    };
    agency_charge = tez 0.5;
    provider_charge = tez 0.5;
    contract_complexity = "Medium-Low";
    certification_status = "Formally verified with Coq";
  }

let frozen_version001 : tmplversion =
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
  let parse_tez_as_mutez str = match String.chop_suffix "tz" str with
    | None -> Result.error ("mulformed spell: "^str^" is not valid amount in tez")
    | Some x -> Q.of_string x |> Q.(mul (of_int 1_000_000)) |> Q.to_int64 |> pure in
  let parse_iso8601 str =
    match Ptime.of_rfc3339 str with
    | Ok (x, _, _) -> Ptime.(x |> to_float_s |> Int64.of_float) |> pure
    | Error _ -> error "malformed spell: %s is not valid iso8601 timestamp" str in
  let parse_genspell = function
    (* expected form: '(frozen0.gen 0.0tz (tz1.. ...) 2000-00-00T00:00:00Z)' *)
    | Sexp.(List [
        (Atom "frozen0.gen");
        (Atom amount_lit);
        (List owners_sexp);
        (Atom unfrozen_lit);
      ]) ->
       owners_sexp |&> parse_tzaddr >>=* fun owners ->
       parse_tez_as_mutez amount_lit >>= fun amount ->
       parse_iso8601 unfrozen_lit >>= fun unfrozen ->
       pure (amount, owners, unfrozen) (* verify unfrozen format *)
    | x -> error "malformed genesis spell: %a" pp_sexp x in
  let parse_invspell = function
    (* expected form: '(frozen0.withdraw 0.0tz tz1..)' *)
    | Sexp.(List [
        (Atom "frozen0.withdraw");
        (Atom amount_lit);
        (Atom beneficiary_lit);
      ]) ->
       parse_tez_as_mutez amount_lit >>= fun amount ->
       parse_tzaddr (Atom beneficiary_lit) >>= fun beneficiary ->
       pure (amount, beneficiary)
    | x -> error "malformed invocation spell: %a" pp_sexp x in
  let genspell_interpreter (module Sdk : TemplateVersionSdk) ~spell =
    (parse_sexp spell >>= parse_genspell >>= fun (amount, owners, unfrozen) ->
     let open Sdk.Michelson_data in
     (amount, of_pair (of_set (owners |&> of_address), (of_timestamp unfrozen))) |> pure)
    |> Result.fold ~ok:Lwt.Infix.(fun (amount, genparam) -> Sdk.pack_data genparam >>= fun packed -> pure (packed, amount) |> Lwt.return)
         ~error:(Lwt.return % Result.error)
  in
  let invspell_interpreter (module Sdk : TemplateVersionSdk) ~spell =
    (parse_sexp spell >>= parse_invspell >>= fun (amount, beneficiary) ->
     let open Sdk.Michelson_data in
     of_pair (of_mutez amount, of_address beneficiary) |> pure)
    |> Result.fold ~ok:Lwt.Infix.(fun param -> Sdk.pack_data param >>= fun packed ->
       Lwt.return (pure ("main", packed, tez 0.))) ~error:(Lwt.return % Result.error)
  in
  let spell_assistants = [

      (* Genesis Spell Assistant *)
      begin
        let fn_fund_amount = "Fund Amount" in
        let fn_frozen_until = "Frozen Until" in
        let fn_fund_owners = "Fund Owners" in
        { salabel = "genesis.basic01";
          form_title = "Frozen Contract Genesis Parameters";
          form_desc = "Enter the genesis parameters for a Frozen contract";
          form_major_button = "Launch", None;
          form_fields = [
              { name = fn_fund_amount; desc = "fund amount";
                typ = `atom `amount; mandated = true;
                placeholder = Some "0";
                doc = Some "the total amount in tez to be locked in this contract";
                requirement_desc = None;
              };
              { name = fn_frozen_until; desc = "fund unlocking timestamp";
                typ = `atom `timestamp; mandated = true;
                placeholder = None;
                doc = Some "the unlocking date and time after which funds could be withdrawn";
                requirement_desc = None;
              };
              { name = fn_fund_owners; desc = "owners of the fund";
                typ = `list (`tzaddr, ","); mandated = true;
                placeholder = None;
                doc = Some "fund owners who could withdraw after Frozen Until";
                requirement_desc = Some "minimum one owner";
              };
            ];
          form_interpreter = (fun (module Sdk) fvs ->
            let amount = List.assoc fn_fund_amount fvs in
            let unfrozen = List.assoc fn_frozen_until fvs in
            let owners =
              List.assoc fn_fund_owners fvs
              |> String.split_on_char ','
              |?> (Fn.negate String.empty_trimmed) in
            match owners with
            | [] -> Error ("There must be at least one fund owner.", [
                          fn_fund_owners, "minimum one owner"
                      ])
            | _ ->
               sprintf "(frozen0.gen %stz (%s) %s)"
                 amount
                 (owners |> String.concat " ")
                 unfrozen
               |> Result.ok)
        }
      end;

      (* Withdraw Spell Assistant *)
      begin
        let fn_amount = "Amount" in
        let fn_beneficiary = "Beneficiary" in
        { salabel = "withdraw.basic01";
          form_title = "Frozen Contract Withdraw Parameters";
          form_desc = "Enter the parameters for performing a withdrawal from a Frozen contract";
          form_major_button = "Withdraw", None;
          form_fields = [
              { name = fn_amount; desc = "withdrawing amount";
                typ = `atom `amount; mandated = true;
                placeholder = Some "0";
                doc = Some "the amount of tez to be withdrawn from the Frozen contract";
                requirement_desc = None;
              };
              { name = fn_beneficiary; desc = "beneficiary of the withdrawal";
                typ = `atom `tzaddr; mandated = true;
                placeholder = None;
                doc = Some "to whom the withdrawn fund should be sent to";
                requirement_desc = None;
              };
            ];
          form_interpreter = (fun (module Sdk) fvs ->
            let amount = List.assoc fn_amount fvs in
            let beneficiary = List.assoc fn_beneficiary fvs in
            sprintf "(frozen0.withdraw %stz %s)" amount beneficiary |> Result.ok)
        }
      end;

    ] in (* XXX *)
  let open struct
        type interpreted_basic = {
            contract : string;
            balance : string;
            owners : string list;
            unfrozen : string;
          }
        [@@deriving yojson]
    end in
  let spirit_interpreter_1 = {
      silabel = "basic.json";
      interpreter = (fun (module Sdk : TemplateVersionSdk) spirit ->
        let open Lwt.Infix in
        let main = spirit.avatars |> List.find (fun av -> av.rclabel = "main") in
        Sdk.unpack_data main.wstore >>= fun store ->
        let open Sdk.Michelson_data in
        let owners, unfrozen =
          to_pair store |> (fun (owners, ts) ->
            to_set owners |&> to_address,
            to_timestamp ts) in
        let unfrozen = Ptime.(
            of_float_s (unfrozen |> Int64.to_float) |> Option.get
            |> to_rfc3339 ~tz_offset_s:0) in
        let json = {
            contract = main.address;
            balance = Q.(div (of_int64 main.balance)
                           (of_int 1_000_000)
                         |> to_float)
                      |> Format.sprintf "%g";
            owners;
            unfrozen;
          } |> interpreted_basic_to_yojson |> Yojson.Safe.to_basic in
        Sdk.unparse_json json |> pure |> Lwt.return)
    } in
  { tmplversion = "tpvFrozenProto001";
    tmplhash = book_of_frozen.entry.tmplhash;
    genspell_interpreter;
    invspell_interpreter;
    spell_assistants;
    spirit_interpreters = [spirit_interpreter_1]; }

module Frozen = struct
  let book_entry = book_of_frozen
  let tmplhash = book_entry.entry.tmplhash
  let tmplversion_001 = frozen_version001
end
