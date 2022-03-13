open Sexplib
module Responder = Bridge_impl.BrokerlibBridgeResponder

let () =
  let modul = "BRD" in
  let info fmt = Log0.info ~modul fmt in
  let verbose fmt = Log0.verbose ~modul fmt in
  let error fmt = Log0.error ~modul fmt in
  let err_logger msg = error "%s" msg in
  let debugging = ArgOptions.has_flag "-debug-bridge" in
  let pp_exn =
    if debugging
    then (
      Printexc.record_backtrace true;
      info "bridge-debug mode on";
      pp_full_exn)
    else pp_exn in
  info "BrokerlibBridge starts";
  print_endline "bridge_ready";
  let rec loop() =
    match Sexp.input_sexp stdin
          |> Sexp.to_string
          |-> (fun s -> if debugging then verbose "input: %s" s)
          |> Responder.handler ~err_logger with
    | output ->
       (output
        |-> (fun s -> if debugging then verbose "output: %s" s)
        |> print_endline);
       loop()
    | exception End_of_file ->
       info "Quitting since EOF received"
    | exception e ->
       error "unknown exception, resuming - %a" pp_exn e;
       loop() in
  loop()
