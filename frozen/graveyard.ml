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

module%scamltypes GraveyardTypes = struct
  open SCaml
  open TemplateTypes

  type store = unit

  type param = unit
  let ccgen : ccgen =
    { genprog = (fun (wstore, initbal) ->
        let main_wfunc : wfunc
          = fun (wparam, wstore) ->
          (* we check param type, and do nothing else *)
          let unpack_param packed =
            match (Obj.unpack packed : param option) with
            | None -> failwith ("graveyard - unable to unpack param")
            | Some param -> param in
          let () = unpack_param wparam in
          [], wstore in
        ["main", main_wfunc, wstore, initbal]
      );
      initprog = (fun _ _ -> [])
    }

end

let () =
  [%scamlvalue GraveyardTypes.ccgen] |> print_endline
