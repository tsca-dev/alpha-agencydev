open Book_intf
open BookTypes
open TmplversionTypes
open Common

let book_of_graveyard : book = {
    entry = {
      bookident = "graveyard";
      bookhash = "bkBookOfGraveyardProto0";
      title = "Book of Graveyard";
      synopsis = "Book of Graveyard is a blackhole contract";
      tmplhash = "tmpL1Q5BaFCgLb9ouyioFT2rP93FRnsreP";
      (* based on tsca-formaldev/a68d9132:graveyard.ccgen *)
      provider = tsca_team.providerident;
      contract_parameters_en = [
        ];
      contract_terms_en = [
          "any one could make a transfer to the originated contract";
          "the fund being transferred to the originated contract can never be recovered";
        ];
      contract_caveats_en = [
          "there is absolutely no way to recover the amount tranferred to the originated contract"
        ];
      specifications = [
          { synopsis = Some "Effective template contents in the TSCA Ccgen format";
            title = "Effective Template";
            url = "https://github.com/kxcteam/tsca-formaldev/blob/master/graveyard.ccg" };
        ]
    };
    agency_charge = tez 0.5;
    provider_charge = tez 0.0;
    contract_complexity = "Low";
    certification_status = "Formally verified with Coq";
  }

let graveyard_version001 : tmplversion =
  let open Sexplib in
  let open MonadOps(ResultWithErrmsg) in
  let error fmt = Format.kasprintf (fun msg -> Result.error msg) fmt in
  let pp_sexp = Sexp.pp_mach in
  let parse_tez_as_mutez str = match String.chop_suffix "tz" str with
    | None -> Result.error ("mulformed spell: "^str^" is not valid amount in tez")
    | Some x -> Q.of_string x |> Q.(mul (of_int 1_000_000)) |> Q.to_int64 |> pure in
  let parse_sexp str =
    try Sexp.of_string str |> pure
    with Failure errmsg -> Result.error errmsg in
  let parse_genspell = function
    (* expected form: '(graveyard0.gen)' *)
    | Sexp.(List [
        (Atom "graveyard0.gen");
      ]) ->
       pure ()
    | x -> error "malformed genesis spell: %a" pp_sexp x in
  let parse_invspell = function
    (* expected form: '(graveyard0.dump 0.0tz)' *)
    | Sexp.(List [
        (Atom "graveyard0.dump");
        (Atom amount_lit);
      ]) ->
       parse_tez_as_mutez amount_lit >>= fun amount ->
       pure (amount)
    | x -> error "malformed invocation spell: %a" pp_sexp x in
  let genspell_interpreter (module Sdk : TemplateVersionSdk) ~spell =
    (parse_sexp spell >>= parse_genspell >>= fun () ->
     let open Sdk.Michelson_data in
     (tez 0., of_bytes Bytes.empty) |> pure)
    |> Result.fold ~ok:Lwt.Infix.(fun (amount, genparam) -> Sdk.pack_data genparam >>= fun packed -> pure (packed, amount) |> Lwt.return)
         ~error:(Lwt.return % Result.error)
  in
  let invspell_interpreter (module Sdk : TemplateVersionSdk) ~spell =
    (parse_sexp spell >>= parse_invspell >>= fun (amount) ->
     let open Sdk.Michelson_data in
     let param =
       of_bytes (Bytes.of_string "\x05\x03\x0b") in
     (amount, param) |> pure)
    |> Result.fold ~ok:Lwt.Infix.(fun (amount, param) -> Sdk.pack_data param >>= fun packed ->
       Lwt.return (pure ("main", packed, amount))) ~error:(Lwt.return % Result.error)
  in
  let spell_assistants = [

      (* Genesis Spell Assistant *)
      begin
        { salabel = "genesis.basic01";
          form_title = "Graveyard Contract Genesis Parameters";
          form_desc = "Confirm you want to generate a Graveyard contract";
          form_major_button = "Launch", None;
          form_fields = [ ];
          form_interpreter = (fun (module Sdk) _fvs ->
            Result.ok "(graveyard0.gen)")
        }
      end;

      (* Dump Spell Assistant *)
      begin
        let fn_amount = "Amount" in
        { salabel = "dump.basic01";
          form_title = "Graveyard Contract Dump Parameters";
          form_desc = "Enter the parameters for performing a dumping to a Graveyard contract";
          form_major_button = "Dump", None;
          form_fields = [
              { name = fn_amount; desc = "dumping amount";
                typ = `atom `amount; mandated = true;
                placeholder = Some "0";
                doc = Some "the amount of tez to be dumped to the Graveyard contract";
                requirement_desc = None;
              };
            ];
          form_interpreter = (fun (module Sdk) fvs ->
            let amount = List.assoc fn_amount fvs in
            sprintf "(graveyard0.dump %stz)" amount |> Result.ok)
        }
      end;

    ] in
  let open struct
        type interpreted_basic = {
            contract : string;
            balance : string;
          }
        [@@deriving yojson]
    end in
  let spirit_interpreter_1 = {
      silabel = "basic.json";
      interpreter = (fun (module Sdk : TemplateVersionSdk) spirit ->
        let main = spirit.avatars |> List.find (fun av -> av.rclabel = "main") in
        let json = {
            contract = main.address;
            balance = Q.(div (of_int64 main.balance)
                           (of_int 1_000_000)
                         |> to_float)
                      |> Format.sprintf "%g";
          } |> interpreted_basic_to_yojson |> Yojson.Safe.to_basic in
        Sdk.unparse_json json |> pure |> Lwt.return)
    } in
  { tmplversion = "tpvGraveyardProto001";
    tmplhash = book_of_graveyard.entry.tmplhash;
    genspell_interpreter;
    invspell_interpreter;
    spell_assistants;
    spirit_interpreters = [spirit_interpreter_1]; }

module Graveyard = struct
  let book_entry = book_of_graveyard
  let tmplhash = book_entry.entry.tmplhash
  let tmplversion_001 = graveyard_version001
end
