module%scamltypes TemplateTypes = struct
  open SCaml

  type ccgen = {
      genprog : (
        bytes*tz ->
        (** [genesis_parameter, init_balance_total] *)
        (string*wfunc*bytes*tz) list
        (** avatar ensemble, each (rclabel, wrapped_code, init_storage) *)
      );
      (** property : [init_balance]'s of returned must sum to [init_balance_total] passed in *)

      initprog : (
        bytes*tz ->
        (** [genesis_parameter, init_balance_total] *)
        (string, address) map ->
        (** physical address of each avatar, as a finmap of [rclabel |-> address] *)
        (string*bytes) list
        (** initialization message per contract, as an assoc_map of [rclabel, init_message] *)
      );
    }
  (** spirit (= "conceptual contract") generator *)
  [@@scaml.noconv]

  and wfunc = bytes*bytes -> operation list*bytes
  (** physical smart contract body type to be wrapped in the wrapper *)
  [@@scaml.noconv]

  and wrapper_store = {
      wrapped_storage : bytes;
      wrapped_wfunc : wfunc;
      wrapped_identity : avatar_identity option;
    }
  [@@scaml.noconv]

  and avatar_identity = {
      avid_broker   : address;
      avid_sprthash : string;
      avid_rclabel  : string;
    }
end

module%scamltypes FrozenTypes = struct
  open SCaml
  open TemplateTypes

  type param =
    (* | Withdraw of *)
    { amount : tz;
      (** amount to be withdrawn *)

      beneficiary : address;
      (** the account to receive the withdrawn fund *)
    }

  type store = {
      fund_owners : address set;
      (** the set of accounts who could each withdraw the fund when it's unfrozen *)

      unfrozen : timestamp;
      (** the date after which the deposit could be withdrawn *)
    }

  let ccgen : ccgen =
    { genprog = (fun (wstore, initbal) ->
        let main_wfunc : wfunc
          = fun (wparam, wstore) ->
          let perform_withdraw ( { amount; beneficiary }) =
            match (Contract.contract beneficiary : unit contract option) with
            | None -> failwith "incorrect or not-supported beneficiary address"
            | Some c -> Operation.transfer_tokens () amount c in

          let validate_invocation
                { amount; _ }
                { fund_owners; unfrozen } =
            if Global.get_amount() > (Tz 0.)
            then failwith "frozen contract cannot accept tokens";
            if not (Set.mem (Global.get_source()) fund_owners)
            then failwith "source of operation is not whitelisted for withdrawal operations";
            if Global.get_now() < unfrozen
            then failwith "deposit is still frozen";
            if Global.get_balance() < amount
            then failwith "requested withdrawal amount exceeds the balance";
            if amount = (Tz 0.)
            then failwith "amount to withdraw must be positive"
            else () in

          let unpack_store packed =
            match (Obj.unpack packed : store option) with
            | None -> failwith ("frozen - unable to unpack param")
            | Some store -> store in

          let param = match (Obj.unpack wparam : param option) with
            | None -> failwith ("frozen - unable to unpack param")
            | Some param -> param in
          let store = unpack_store wstore in
          validate_invocation param store;
          let op = perform_withdraw param in
          [op], wstore in
        ["main", main_wfunc, wstore, initbal]
      );
      initprog = (fun _ _ -> [])
    }

end

module%scamlcontract FrozenMain = struct
  open FrozenTypes
  open TemplateTypes
  open SCaml

  let unpack_store packed =
    match (Obj.unpack packed : store option) with
    | None -> failwith ("frozen - unable to unpack param")
    | Some store -> store

  let main_wfunc : wfunc
    = fun (wparam, wstore) ->
    let perform_withdraw ( { amount; beneficiary }) =
      match (Contract.contract beneficiary : unit contract option) with
      | None -> failwith "incorrect or not-supported beneficiary address"
      | Some c -> Operation.transfer_tokens () amount c in

    let validate_invocation
          { amount; _ }
          { fund_owners; unfrozen } =
      if Global.get_amount() > (Tz 0.)
      then failwith "frozen contract cannot accept tokens";
      if not (Set.mem (Global.get_source()) fund_owners)
      then failwith "source of operation is not whitelisted for withdrawal operations";
      if Global.get_now() < unfrozen
      then failwith "deposit is still frozen";
      if Global.get_balance() < amount
      then failwith "requested withdrawal amount exceeds the balance";
      if amount = (Tz 0.)
      then failwith "amount to withdraw must be positive"
      else () in

    let unpack_store packed =
      match (Obj.unpack packed : store option) with
      | None -> failwith ("frozen - unable to unpack param")
      | Some store -> store in

    let param = match (Obj.unpack wparam : param option) with
      | None -> failwith ("frozen - unable to unpack param")
      | Some param -> param in
    let store = unpack_store wstore in
    validate_invocation param store;
    let op = perform_withdraw param in
    [op], wstore

  let entrypoint (param : param) (store : store) =
    let wparam, wstore = Obj.(pack param, pack store) in
    let ops, wstore = main_wfunc (wparam, wstore) in
    ops, unpack_store wstore
  [@@entry]
end

let initst :
      owners:(string list) ->
      unfrozen:string ->
      FrozenTypes.store
  = fun ~owners ~unfrozen ->
  let open SCaml in
  FrozenTypes.{
      fund_owners = Set (owners |> List.map (fun x -> Address x));
      unfrozen = Timestamp unfrozen;
  }

let pp_timestamp ppf ts =
  let (>>=) = Option.bind in
  (match int_of_string_opt ts >>= fun ts ->
         Ptime.Span.of_int_s ts |> Ptime.of_span with
   | Some t -> Ptime.pp_human () ppf t
   | None ->
      match Ptime.of_rfc3339 ~strict:false ts with
      | Ok (t, _, _) -> Ptime.pp_human () ppf t
      | Error _ -> Format.fprintf ppf "error parsing RFC3339 format");
  Format.fprintf ppf " ; orig='%s'" ts

let explain_store : FrozenTypes.store -> unit =
  fun { fund_owners = Set (fund_owners);
        unfrozen = Timestamp unfrozen } ->
  let open Format in
  print_flush();
  print_string
    "[frozen.ml contract]"; print_newline();
  print_string
    "Fund Owners: ["; print_newline();
  print_string "  ";
  print_flush();
  open_box 2;
  fund_owners
  |> List.iter (fun (SCaml.Address acc) ->
         print_string acc;
         print_string "; "; print_cut());
  close_box(); print_cut(); print_string "]";
  print_newline();
  print_string
    "Unfrozen Timestamp: ";
  pp_timestamp std_formatter unfrozen;
  print_newline();
  print_flush()
  
module Args = struct
  let tzout : string option ref = ref None
  let printing : [`Nothing |
                  `ParameterType |
                  `StorageType |
                  `InitStorage |
                  `ParamWithdraw |
                  `InterpStorage of string |
                  `ContractCode |
                  `CcgenCode
                 ] ref = ref `Nothing

  let toprint x () = printing := x

  module GenesisParams = struct
    let owners    : string option ref = ref None
    let unfrozen : string option ref = ref None
    let set r x =
      r := Some x;
      printing := `InitStorage
    let get r = Option.get !r
  end

  module InvokWithdrawParams = struct
    let amount : float option ref = ref None
    let beneficiary : string option ref = ref None
    let set r x =
      r := Some x;
      printing := `ParamWithdraw
    let get r = Option.get !r
  end

  let speclist = [
      ("-tzout", Arg.String (fun x -> tzout := Some x;
                                      if x = "-" then printing := `ContractCode),
       "<out> specify to write the contract code; '-' means stdout");
      ("-ccgen", Arg.Unit (toprint `CcgenCode),
       "prints ccgen lambda body to stdout");
      ("-paramtype", Arg.Unit (toprint `ParameterType),
       "print contract Michelson parameter type");
      ("-storagetype", Arg.Unit (toprint `StorageType),
       "print contract Michelson strage type");
      ("-invok-withdraw", Arg.Tuple InvokWithdrawParams.[
           (Arg.Float (set amount));
           (Arg.String (set beneficiary))
       ], "<amount> <beneficiary> print contract argument in Michelson to perform a Withdraw");
      ("-interp-storage", Arg.String (fun st -> printing := (`InterpStorage st)),
       "<storage> interpret and describe the storage value in Michelson");
      ("-initstorage", Arg.Tuple GenesisParams.[
           (Arg.String (set owners));
           (Arg.String (set unfrozen));
       ], "<fund-owners> <unfrozen-date> \
           synthesize the initial storage for the contract; \
           <fond-owners> in comma separated list");
    ]

  let usage() = Arg.usage speclist "frozen.ml: Frozen smart-contract coordinator\n\
                                    please specify an argument"
  let () = Arg.parse speclist print_endline "frozen.ml"
end

let () =
  let open Args in
  (match !tzout with
   | Some out ->
      let ch =
        if out = "-" then stdout
        else open_out out
      in output_string ch [%scamlcontract FrozenMain];
         flush ch
   | _ -> ());
  (match !printing with
   | `ParameterType ->
      [%scamltype: FrozenTypes.param]#tztype |> print_endline
   | `StorageType ->
      [%scamltype: FrozenTypes.store]#tztype |> print_endline
   | `InitStorage ->
      let open GenesisParams in
      let iv = initst
                 ~owners:((get owners) |> Str.split_delim (Str.regexp ","))
                 ~unfrozen:(get unfrozen) in
      [%scamltype: FrozenTypes.store]#convert iv
      |> print_endline
   | `ParamWithdraw -> 
      let open InvokWithdrawParams in
      let open FrozenTypes in
      [%scamltype: FrozenTypes.param]#convert
        { amount = Tz (get amount);
          beneficiary = Address (get beneficiary);
        } |> print_endline
   | `InterpStorage st -> begin
       try
         [%scamltype: FrozenTypes.store]#revert st
         |> Option.get
         |> explain_store
       with _ -> Format.eprintf "Michelson parsing error"
     end
   | `ContractCode ->
      () (* already handled above *)
   | `CcgenCode ->
      [%scamlvalue FrozenTypes.ccgen] |> print_endline
   | `Nothing -> usage()
  )
