open Jsutils

external sset : string -> int -> char -> unit = "%string_safe_set"

let [@warning "-33"] helpers =
  let shell_quote s =
    (* adopted from ocaml/stdlib/filename.ml:generic_quote *)
    (**************************************************************************)
    (*                                                                        *)
    (*                                 OCaml                                  *)
    (*                                                                        *)
    (*           Xavier Leroy and Damien Doligez, INRIA Rocquencourt          *)
    (*                                                                        *)
    (*   Copyright 1996 Institut National de Recherche en Informatique et     *)
    (*     en Automatique.                                                    *)
    (*                                                                        *)
    (*   All rights reserved.  This file is distributed under the terms of    *)
    (*   the GNU Lesser General Public License version 2.1, with the          *)
    (*   special exception on linking described in the file LICENSE.          *)
    (*                                                                        *)
    (**************************************************************************)
    let quotequote = "'\\''" in (* hx - fix for *nix *)
    let l = String.length s in
    let b = Buffer.create (l + 20) in
    Buffer.add_char b '\'';
    for i = 0 to l - 1 do
      if s.[i] = '\''
      then Buffer.add_string b quotequote
      else Buffer.add_char b  s.[i]
    done;
    Buffer.add_char b '\'';
    Buffer.contents b in
  let may_escape_for_shell =
    let unsafe = Re.(Perl.re {|[^a-zA-Z0-9,._+:@%/-]|} |> compile) in
    fun str ->
    match Re.exec_opt unsafe str with
    | None -> str
    | Some _ -> shell_quote str in
  let open Agency_apis.Aii_dir in
  let open AgencyTypes in
  let open AgencyInternalInterface.Proto0 in
  let open Jsaii.PrivateHelpers in
  let (&>) f func x = func (f x) in
  let formatCliInstruction : instruction_line list -> string list
    = fun lines ->
    let line = function
      | InstructionComment str ->
         str |> String.split_on_char '\n'
         |&> sprintf "# %s"
      | InstructionParameter (param, desc) ->
         sprintf "  $%s : @[%s@]" param desc
         |> String.split_on_char '\n'
         |&> (fun line -> sset line 0 '#'; line)
      | TezosClientCommand parts ->
         parts |&> (function
                    | (true, param) -> sprintf "$%s" param
                    | (false, arg) -> may_escape_for_shell arg)
         |> String.concat " " |> List.pure in
    lines |> List.fmap line in
  let js_formatCliInstructionIntoLines =
      (List.map instruction_line_of_yojson % Yojson.Safe.Util.to_list) %> Result.concat
      %~?> formatCliInstruction &> (fun lines ->
        `List (lines |&> (fun x -> `String x))
      ) &> js_of_yojson in
  let js_formatCliInstructionIntoString =
      (List.map instruction_line_of_yojson % Yojson.Safe.Util.to_list) %> Result.concat
      %~?> formatCliInstruction &> String.concat "\n" &> jsstr' in
  object%js
    val formatCliInstructionIntoLines = js_formatCliInstructionIntoLines
    val formatCliInstructionIntoString = js_formatCliInstructionIntoString
  end

let () =
  let open Jsutils in
  info "%s loading" __FILE__;
  let open Jsaii.AiiJsBridge in
  Js.export "TSCAInternalInterface" (object%js
        val _Helpers = helpers
        val _RefMaster = RefMaster.jsintf
        val _TezosUtils = TezosUtils.jsintf
        val _IndexerLevel0 = IndexerLevel0.jsintf
        val _Proto0 = Proto0.jsintf
        val _InfoBank = InfoBank.jsintf
      end);
  info "%s loaded" __FILE__;
