let sprintf fmt = Format.asprintf fmt
let eprintf fmt = Format.eprintf fmt
let pp_sexp = Sexplib.Sexp.pp_hum

module Globals0 = struct
  let program_starts = Unix.gettimeofday()
  let upstream_conn_timeout = 1. (* sec *)
end

module Logging = struct
  open Format
  let logging_formatter = err_formatter
  let log_raw fmt = fprintf logging_formatter fmt
  let log ~label fmt =
    let t = Unix.gettimeofday() -. Globals0.program_starts in
    log_raw "[%s :%.3f] @[<hov 2>" label t;
    kfprintf (fun ppf -> fprintf  ppf "@]@.")
      logging_formatter fmt
  let access fmt = log ~label:"ACCESS" fmt
  let info fmt = log ~label:"INFO" fmt
  let warn fmt = log ~label:"WARN" fmt
  let debug fmt = log ~label:"DEBUG" fmt
  let error fmt = log ~label:"ERROR" fmt
end open Logging

module PrivateUtils = struct
  let categorize_path path =
    match Sys.(file_exists path, file_exists path && is_directory path) with
    | false, _ -> `Non_exsists
    | true, false -> `File
    | true, true -> `Directory
  let may_replace_assoc (k, v) l =
    if List.mem_assoc k l then l
    else (k, v) :: (List.remove_assoc k l)
  let dir_dim_regexp = Str.regexp_string Filename.dir_sep
end open PrivateUtils

module Globals = struct
  include Globals0

  let client_scheme = ref None
  let agency_base =
    ArgOptions.(get_option_d (StringOption "-agency-base")
                  "https://agency.tsca")
  let unrestricted_cors = ArgOptions.has_flag "-dev-cors"

  let server_port = ArgOptions.(get_option_d (IntOption "-port") 8008)

  let bridge_prog = ArgOptions.(
      get_option_d (StringOption "-bridge")
        ("../proto_pinned/broker/bridge.exe"))

  let bookapp_base_domain =
    ArgOptions.(get_option_d (StringOption "-bookapp-domain") "bookapp.tsca")
  let bookapp_port = ArgOptions.(get_option_d (IntOption "-bookapp-port") server_port)

  let bookapp_repository =
    ArgOptions.(get_option (StringOption "-bookapp-repository"))
    |> Option.v' (fun () -> invalid_arg "you must specify -bookapp-repository")

  let static_asset_root =
    ArgOptions.(get_option (StringOption "-static-assets"))

  let get_client_scheme() = match !client_scheme with
    | None -> "http"
    | Some s -> s
  
  let parse_bookapp_domain str =
    str |> String.chop_suffix ("."^bookapp_base_domain)

  let agency_index_path =
    ArgOptions.(get_option (StringOption "-index"))
    |> Option.v' (fun () ->
           let path = Filename.temp_file "tsca_agency_server" ".index" in
           at_exit (fun () -> Sys.remove path);
           path)

  let () = Nocrypto_entropy_lwt.initialize() |> Lwt_main.run

  let __global_nounce = lazy (
    let byte_size = 32 in
    Nocrypto.Rng.generate byte_size
    |> Cstruct.to_bytes
    |> Commons.hex_of_bytes)
  let global_nounce() = Lazy.force __global_nounce

  let admin_http_token =
    let gen_token ?byte_size:(bs=12) () =
      Nocrypto.Rng.generate bs
      |> Cstruct.to_bytes
      |> Commons.hex_of_bytes in
    let raw_token = ArgOptions.(
      get_option (StringOption "-admin-http-token")
      |> Option.v' gen_token) in
    "admin:"^raw_token

  (* list of broker contracts managed by this agency *)
  let brokers =
    ArgOptions.(get_option_exn (StringOption "-brokers"))
    |> String.split_on_char ','

  let upstream_mappings :
        target:string ->
        (string (* upstream *) * string (* url *)) option =
    match ArgOptions.(get_option (StringOption "-upstream")) with
    | None -> fun ~target:_ -> None
    | Some maps -> (
      let malformed_argument() =
        error "malformed -upstream: must be of form '<target pattern> => <upstream pattern>'@\n\
               e.g. '/api/... => http://localhost:4321/...'";
        exit 2 in
      let parse_single map =
        match Str.split_delim (Str.regexp_string " => ") map with
        | [target_patt; upstream_patt] ->
           let split = Str.(split_delim (regexp_string "...")) in
           (match split target_patt, split upstream_patt with
            | [target_before; target_after], [path_before; path_after] ->
               let target_regexp =
                 let open Str in
                 sprintf "%s\\(.*\\)%s" (quote target_before) (quote target_after)
                 |> regexp in
               fun ~target ->
               let open Str in
               if string_match target_regexp target 0 then (
                 let subtarget = matched_group 1 target in
                 Some (upstream_patt, path_before^subtarget^path_after)
               ) else None
            | _ -> malformed_argument())
        | _ -> malformed_argument() in
      let mappings =
        String.split_on_char ';' maps
        |&> String.trim
        |&> parse_single in
      fun ~target ->
      let rec loop = function
        | [] -> None
        | head :: rest ->
           (match head ~target with
            | Some r -> Some r
            | None -> loop rest) in
      loop mappings)

  let () =
    info "Agency index path: %s" agency_index_path;
    info "admin_http_token: %s" admin_http_token
end

open Lwt.Infix
open Httpaf
open Httpaf_lwt_unix

let create_tezos_context ?base_dir () =
  let open ArgOptions in
  let open Tznode in
  let logger =
    if has_flag "-log-rpc-with-tznode" then `FullStderr else `Null in
  let endpoint = get_option (StringOption "-endpoint") in
  let chain, block =
    let open Bcutils in
    get_option (StringOption "-chain") |> Option.fold ~none:`Main ~some:chain_of_string,
    get_option (StringOption "-block") |> Option.fold ~none:(`Head 0) ~some:block_of_string in

  client_context ?base_dir ?endpoint ~logger () >>= fun cctxt ->
  Lwt.return (cctxt, chain, block)

let cctxt, chain, block =
  let base_dir = ArgOptions.(get_option (StringOption "-base-dir")) in
  create_tezos_context ?base_dir () |> Lwt_main.run

(* NB that we have 2 "bridges", it's merely some sort of naming confliction *)
module Apis = Aii_dir.AgencyInternalInterface
module Bridge =
  Api_directory.ApiHandlerBridge
    (Apis)
    (struct include Lwt let pure x = return x end )

let brokerlib_bridge = Indexerlib.make_bridge Globals.bridge_prog
module BrokerlibBridge = (val brokerlib_bridge)

module Content_digest = struct
  include Digestif.MD5
  let hex_hash_of_string str =
    feed_string empty str |> get |> to_hex
end

module StaticAssets = struct
  (* XXX lwt-ify *)

  type asset_entry = {
      mutable contents : Bigstringaf.t;
      mutable len   : int;
      mutable stats : Unix.stats;
      mutable digest : Content_digest.t;
      mutable digest_hex : string;
      name : string;
      source : string;
      fd : Unix.file_descr;
      media_type : string;
    }

  type asset = asset_entry lazy_t

  let guess_media_type path =
    let basename = Filename.(path |> basename |> String.lowercase_ascii) in
    let ext = basename |> Filename.extension in
    match basename, ext with
    | _, ".html" | _, ".htm" -> "text/html"
    | _, ".txt" | _, ".md" -> "text/plain"
    | _, ".css" -> "text/css"
    | _, ".js" -> "text/javascript"
    | _, ".jpeg" -> "image/jpeg"
    | _, ".jpg" -> "image/jpg"
    | _, ".png" -> "image/png"

    | "dune", _ | "dune-project", _ | "ml", _
      -> "text/plain"

    | _ ->
       warn "unknown media type for file: %s (ext=%s)" basename ext;
       "application/octet-stream"

  let do_load fd stats =
    let len = stats.Unix.st_size in
    let contents : Bigstringaf.t = Bigarray.(
        Unix.map_file fd Char C_layout false [|len|]
        |> array1_of_genarray) in
    let digest = Content_digest.(
        feed_bigstring empty contents |> get) in
    let digest_hex = Content_digest.to_hex digest in
    len, contents, digest, digest_hex

  let asset_contents ?mtype path : asset = lazy (
    let fd = Unix.openfile path [O_RDONLY] 0o640 in
    let stats = Unix.fstat fd in
    let len, contents, digest, digest_hex =
      do_load fd stats in
    let source =
      let open Filename in
      if is_relative path
      then (sprintf "file://./%s" path)
      else (sprintf "file://%s" path) in
    let name = path in
    info "asset %s loaded, digest = %s"
      name digest_hex;
    at_exit (fun () -> Unix.close fd);
    let mtype = Option.v' (fun () -> guess_media_type path) mtype in
    { contents; len; stats; media_type = mtype;
      digest; digest_hex; fd; name; source })

  let need_reload asset =
    let entry = Lazy.force asset in
    let stats' = Unix.fstat entry.fd in
    entry.stats.st_mtime < stats'.st_mtime

  let mtime asset =
    let fd = (Lazy.force asset).fd in
    Unix.(fstat fd).st_mtime

  let get asset =
    let entry = Lazy.force asset in
    let stats' = Unix.fstat entry.fd in
    if entry.stats.st_mtime < stats'.Unix.st_mtime then (
      let old_digest = entry.digest_hex in
      info "reloading asset %s with digest %s" entry.name old_digest;
      let len, contents, digest, digest_hex =
        do_load entry.fd stats' in
      info "asset %s reloaded, digest : %s => %s"
        entry.name old_digest digest_hex;
      entry.len <- len;
      entry.contents <- contents;
      entry.stats <- stats';
      entry.digest <- digest;
      entry.digest_hex <- digest_hex;
    ); entry

  let webapp_spa_html =
    let open Kxclib.ArgOptions in
    match get_option (StringOption "-serve-agency-spa") with
    | None -> None
    | Some spa ->
       Some (asset_contents ~mtype:"text/html" spa)

  let aii_jslib_js =
    asset_contents ~mtype:"text/javascript"
      "agency/aii-jslib.js"
  let tryout_aii_html =
    asset_contents ~mtype:"text/html"
      "agency/tryout-aii.html"
  let bil_jslib_js =
    asset_contents ~mtype:"text/javascript"
      "agency/bil-jslib.js"
  let tryout_bil_html =
    asset_contents ~mtype:"text/html"
      "agency/tryout-bil.html"
  let agency_index =
    let open MonadOps(Option) in
    asset_contents ~mtype:"application/octet-stream"
      Globals.agency_index_path
end

module AssetCache = struct
  (* XXX - change to LRU or alike *)
  module Private = struct
    let cache = Hashtbl.create 100
  end open Private

  open StaticAssets

  let get ?mtype path =
    let mtype = match mtype with
      | Some typ -> typ
      | None -> guess_media_type path in
    match Hashtbl.find_opt cache path with
    | Some asset -> asset
    | None ->
       let asset = asset_contents ~mtype path in
       Hashtbl.add cache path asset;
       asset
end

module AgencyIndex = struct
  open Indexerlib
  include IndexedTypes
  include IndexV0

  type cache = {
      index : t;
      mtime : float;
    }
  let cache = ref (None : cache option)

  let bridge = brokerlib_bridge

  exception AgencyIndexNotAvailable

  let reload() =
    let open StaticAssets in
    let open MonadOps(Option) in
    let { contents; len; stats; _ } = get agency_index in
    let bytes = Bytes.init len (fun i ->
                    Bigarray.Array1.unsafe_get contents i) in
    Serialization.from_bytes bytes >>= fun index ->
    cache := Some { index; mtime = stats.st_mtime};
    Some index

  let index_opt() =
    let open MonadOps(Option) in
    match !cache with
    | None -> reload()
    | Some { index; mtime } ->
       if StaticAssets.(mtime agency_index) > mtime
       then reload()
       else Some index
  let index() =
    match index_opt() with
    | None -> raise AgencyIndexNotAvailable
    | Some index -> index

  let save_and_cache_index ~mtime index =
    Log0.info ~modul:"IDX" "cache saved";
    cache := Some { index; mtime };
    with_output_file Globals.agency_index_path
      (fun ch ->
        Serialization.to_bytes index
        |> output_bytes ch)

  let last_reindex_requested = ref 0. (* unix timestamp in seconds *)

  let reindex
        ?on_finish:(on_finish=constant ())
        ?on_error:(on_error=constant ())
        ?request_desc
        () =
    (* XXX - real async *)
    let workload() =
      let request_desc = match request_desc with
        | None -> ""
        | Some info -> " via "^info in
      let mtime0 = Unix.gettimeofday() in
      if mtime0 -. !last_reindex_requested < 5.
      then (
        info "re-index skipped: request too frequent";
        (match index_opt() with
         | Some index -> on_finish index
         | None -> on_error "no-index available yet");
        Lwt.return_unit
      ) else (
        last_reindex_requested := mtime0;
        info "re-indexing initialized: %s" request_desc;
        Lwt.catch (fun () ->
            (* Lwt_preemptive.detach *)
            (fun () ->
              create_tezos_context() >>= fun (cctxt, chain, block) ->
              let index =
                Indexer_v0.fresh_index cctxt
                  ~bridge
                  ~logger:(constant ())
                  ~chain ~block ~brokers:Globals.brokers in
              let mtime = Unix.gettimeofday() in
              info "re-indexing finished";
              index >>= fun index ->
              on_finish index;
              (mtime,index) |> Lwt.return
            ) () >>= fun (mtime, index) ->
            save_and_cache_index ~mtime index |> Lwt.return
          )
          Printexc.(fun exn ->
          let msg = sprintf "re-indexing failed: %s@;%s"
                      (to_string exn)
                      (get_raw_backtrace() |> raw_backtrace_to_string) in
          error "%s" msg;
          on_error msg |> Lwt.return)) in
    workload() |> ignore
end

let with_index f =
  f (AgencyIndex.index())

let strip_tzresult_lwt ex =
  match Lwt_main.run ex with
  | Ok x -> x
  | Error e ->
     Format.eprintf "%a" Tzutils.Tm.pp_print_trace e;
     Stdlib.failwith "tzresult = Error"

let supported_network =
  let open Agency_types in
  let chain_id =
    let chain_id, _ =
      Tznode.get_branch ~chain ~block cctxt
      |> strip_tzresult_lwt in
    let tr = Tezos_protocol_011_PtHangz2.Protocol.Environment.Chain_id.to_b58check in
    tr chain_id in
  { netident = "testnet";
    chain_id; }

let auth_admin reqd =
  let open MonadOps(Option) in
  Option.value ~default:false begin
      (Reqd.request reqd).headers
      |> Fn.flip Headers.get "Authorization"
      >>= String.chop_prefix "Bearer "
      >>= fun token ->
      let expected_token = Globals.admin_http_token in
      (* we use a "hash-then-compare" scheme to mitigate possible timing attack *)
      let hash_eq, hash =
        Digestif.SHA256.(equal, digest_string) in
      ((String.length token <= 3*(String.length expected_token))
       (* ^this clause is added to prevent wasting too much server resource
        * on hashing long tokens while not to reveal too much info about the
        * length of expected_token through the timing channel.
        * the factor of [3] was arbitrarily chosen *)

       && (hash_eq (hash token) (hash expected_token)))
      |> Option.some
    end

let additional_headers ?target headers =
  let may_replace (header, value) headers =
    if List.mem_assoc header headers then headers
    else (header, value) :: (List.remove_assoc header headers) in
  let may_replace_if cond =
    if cond then may_replace else constant identity in
  headers
  |> may_replace_if Globals.unrestricted_cors
       ("Access-Control-Allow-Origin", "*")
  |> (match target with
      | Some target when target |> String.starts_with "/widgets/o/" ->
         may_replace
           ("Content-Security-Policy",
            if ArgOptions.has_flag "-loose-widgeto-csp"
            then "frame-ancestors *; sandbox allow-scripts"
            else sprintf "frame-ancestors *.%s:%d; sandbox allow-scripts"
                   Globals.bookapp_base_domain Globals.bookapp_port)
      | Some target when target |> String.starts_with "/widgets/" ->
         may_replace
           ("Content-Security-Policy",
            sprintf "frame-ancestors 'self'; sandbox allow-scripts")
      | _ -> 
         may_replace ("Content-Security-Policy", "frame-ancestors 'none';"))

let stream_contents_with_prefix
      ?prefix
      ?headers:(headers=[])
      ?status:(status=`OK)
      ?cache_control
      ~contents reqd =
  let { StaticAssets.contents; len = clen;
        media_type; digest_hex; _ } = contents in
  let prefix, etag = match prefix with
    | None -> "", digest_hex
    | Some str ->
       str, Content_digest.(
         sprintf "%s-%s" digest_hex
           (hex_hash_of_string str)) in
  let open MonadOps(Option) in
  let { Request.meth; target; headers = reqheaders; _ } =
    Reqd.request reqd in
  match Headers.get reqheaders "If-None-Match" with
  | Some client_etag when (client_etag = etag) ->
     let headers = Headers.of_list ([
                       "Content-Length", "0";
                       "Etag", etag] |> additional_headers ~target) in
     let resp = Response.create ~headers `Not_modified in
     Reqd.respond_with_string reqd resp ""
  | _ -> begin
      let len = clen + String.length prefix in
      let headers =
        let may_replace (header, value) headers =
          if List.mem_assoc header headers then headers
          else (header, value) :: headers in
        headers
        |> may_replace ("Content-Length", len |> string_of_int)
        |> may_replace ("Content-Type", media_type)
        |> may_replace ("ETag", etag)
        |> additional_headers ~target
        |> Headers.of_list in
      let headers =
        let verb = match cache_control with
          | Some `No_store -> Some "no-store"
          | Some `No_cache -> Some "no-cache"
          | Some `Public -> Some "public"
          | Some `Private | None -> Some "private" in
        match verb with
        | None -> headers
        | Some verb ->
           Headers.add_unless_exists headers
             "Cache-Control" verb in
      let resp = Response.create ~headers status in
      let body =
        Reqd.respond_with_streaming reqd resp
          ~flush_headers_immediately:true in
      if meth <> `HEAD then (
        Body.write_string body prefix;
        Body.schedule_bigstring body contents);
      Body.flush body (fun () -> Body.close_writer body)
    end

let respond_reqd_with_string
      ?headers:(headers=[])
      ?status:(status=`OK)
      reqd str =
  let headers = ("content-length", String.length str |> string_of_int) :: headers in
  let headers = headers |> additional_headers ~target:(Reqd.request reqd).target
                |> Headers.of_list in
  let resp = Response.create ~headers status in
  Reqd.respond_with_string reqd resp str

let respond_with_404 ?uri reqd =
  let uri = sprintf "%a" (Option.pp Uri.pp) uri in
  warn "Route not found: %s" uri;
  let resp = sprintf "Error 404 : requested route not found (%s)" uri in
  let headers =
    Headers.of_list ([
          "Content-Length", string_of_int (String.length resp)
        ]) in
  Reqd.respond_with_string reqd (Response.create ~headers `Not_found) resp

let respond_with_503 ?uri ?message reqd =
  let uri = sprintf "%a" (Option.pp Uri.pp) uri in
  let message = sprintf "%a" (Option.pp pp_string) message in
  warn "Route not found: %s" uri;
  let resp = sprintf "Error 503 : server internal error (%s) : %s" uri message in
  let headers =
    Headers.of_list ([
          "Content-Length", string_of_int (String.length resp)
        ]) in
  Reqd.respond_with_string reqd (Response.create ~headers `Internal_server_error) resp

let async_handle ?exnfunc reqd workload =
  let exnfunc =
    let default = fun exn ->
      let msg = "Server internal error: contact support please" in
      Printexc.(
        error "500 handling api: %s@;%a"
          (to_string exn)
          (List.pp pp_string)
          (get_raw_backtrace() |> raw_backtrace_to_string
           |> String.split_on_char '\n'));
      respond_reqd_with_string ~status:`Internal_server_error reqd msg in
    match exnfunc with
    | None -> default
    | Some func -> (fun exn ->
       match func exn with
       | () -> ()
       | exception e -> default e) in
  Lwt.catch workload (exnfunc &> Lwt.return) |> ignore

let request_host req =
  let { Request.headers; _ } = req in
  Headers.get_exn headers "Host"

let getting_method : Method.t -> bool = function
  | `GET | `HEAD -> true
  | _ -> false

let index_info index =
  let idx = index in
  let open AgencyIndex in
  match idx with
  | None -> "index not available"
  | Some index ->
     block_info index |> sexp_of_block_info
     |> sprintf "index available, where block_info being@;%a" pp_sexp

let handle_upstream ?remote_ip ~upstream ~url reqd =
  let module Client = Cohttp_lwt_unix.Client in
  let url = url |> Uri.of_string in
  let req = Reqd.request reqd in
  let with_timeout ~timeout v progn =
    let timeout =
      Lwt_unix.sleep timeout >>= fun () ->
      Lwt.return v in
    Lwt.pick [progn; timeout] in
  let host = Headers.get_exn req.headers "host" in
  let headers =
    let remote_ip = match remote_ip with
      | None -> identity
      | Some ip -> may_replace_assoc ("x-forwarded-for", ip) in
    req.headers
    |> Headers.to_list
    |&> (fun (k,v) -> String.lowercase_ascii k, v)
    |> List.remove_assoc "host"
    |> may_replace_assoc ("x-forwarded-host", host)
    |> remote_ip
    |> Cohttp.Header.of_list in
  let body, start_body_transfer =
    let reqbody = Reqd.request_body reqd in
    let s, wr = Lwt_stream.create() in
    let rec on_read bs ~off ~len =
      Bigstringaf.substring bs ~off ~len
      |> Option.some |> wr;
      Body.schedule_read reqbody ~on_read ~on_eof
    and on_eof() = wr None in
    `Stream s, (fun () ->
      Body.schedule_read reqbody ~on_read ~on_eof) in
  match req.meth with
  | #Method.standard as meth ->
     let progn() =
       let timeout = Globals.upstream_conn_timeout in
       with_timeout ~timeout None
         (Client.call ~body ~headers meth url
          |> Fn.tap (fun _ -> start_body_transfer())
          >|= Option.some) >>= function
       | None ->
          error "upstream connection timeout; upstream=%s; url=%s"
            upstream (Uri.to_string url);
          respond_reqd_with_string ~status:`Internal_server_error reqd
                   "timeout making connection to upstream";
          Lwt.return_unit
       | Some (resp, ubody) ->
         let stream = Cohttp_lwt.Body.to_stream ubody in
         let { Cohttp.Response.headers; status; _ } = resp in
         let headers = headers |> Cohttp.Header.to_list |> Headers.of_list in
         let status = Cohttp.Code.code_of_status status |> Status.of_code in
         let cbody =
           Response.create ~headers status
           |> Reqd.respond_with_streaming reqd in
         stream |> Lwt_stream.iter (fun chunk ->
                       Body.write_string cbody chunk) >>= fun () ->
         Lwt_stream.closed stream >>= fun () ->
         Body.flush cbody (fun () -> Body.close_writer cbody);
         Lwt.return_unit in
     async_handle reqd progn
  | _ ->
     error "unsupported HTTP method in handle_upstream: %a"
       Method.pp_hum req.meth;
     respond_reqd_with_string ~status:`Not_implemented reqd
       (sprintf "http request method (%a) not supported"
          Method.pp_hum req.meth)

let serve_bil_jslib sprthash reqd =
  let open Bil_commons in
  let endpoint = "/" in
  let bil_info = { sprthash;
                   network = supported_network;
                   agency_base = Globals.agency_base } in
  let prefix =
    sprintf "var __tsca_agency_api_endpoint = \"%s\";@.\
             var __tsca_bil_info = %s;@."
      endpoint (json_of_bil_info bil_info) in
  stream_contents_with_prefix reqd
    ~prefix ~cache_control:`Public
    (* NB - should change to private caching if access token included *)
    ~contents:StaticAssets.(get bil_jslib_js)

let handle_agency_request reqd =
  let ({ Request.meth; target; _ } as req) = Reqd.request reqd in
  let host = request_host req in
  let uri = (Globals.get_client_scheme())^"://"^
              host^target |> Uri.of_string in
  let serve_static_asset path =
    let asset = AssetCache.get path in
    (* let media_type = guess_media_type path in
     * let asset = Assets.asset_of_path ~media_type path in *)
    let contents = StaticAssets.get asset in
    stream_contents_with_prefix reqd ~contents in
  let calc_static_asset_path() =
    Globals.static_asset_root
    |> Option.map (fun root ->
          (* as a security measure, we'd like to check that
           target does not has components referring to parent folders *)
          if Str.split_delim dir_dim_regexp target
             |> List.mem Filename.parent_dir_name then (
            None)
          else (
            (* XXX we probably want to normalize the path *)
            let path = Filename.concat root target in
            match categorize_path path with
            | `File -> Some path
            | `Directory | `Non_exsists -> None
         )) in
  match target with
  | "/_tscadev/re-index" when meth = `POST && auth_admin reqd ->
     let on_finish index = 
         let res = sprintf "re-index done. %s" (index_info (Some index)) in
         respond_reqd_with_string reqd res in
     let on_error msg =
       error "%s" msg;
       respond_reqd_with_string ~status:`Internal_server_error reqd msg in
     let request_desc = sprintf "admin http-request at %s" target in
     AgencyIndex.reindex ~on_finish ~on_error ~request_desc ()
  | "/_tscadev/index-info" when getting_method meth ->
     respond_reqd_with_string reqd (AgencyIndex.index_opt() |> index_info)
  | "/_tscadev/tmp/indexed" when getting_method meth ->
     let open AgencyIndex in
     let index = index() in
     let templates = list_templates index in
     let spirits = index.spirits in
     let res =
       sprintf "%d brokers:@\n%a@\n@\n%d templates:@\n%a@\n@\n%d spirits indexed:@\n%a"
         (Array.length index.brokers)
         (List.pp pp_sexp)
         (index.brokers |> Array.to_list |&> sexp_of_broker_entry)

         (List.length templates)
         (List.pp pp_sexp)
         (templates |&> sexp_of_template_entry)

         (Array.length spirits)
         (List.pp pp_sexp)
         (Array.to_list spirits
          |&> sexp_of_spirit_entry)
     in
     respond_reqd_with_string reqd res
  | "/_tscadev/tryout-aii.html" when getting_method meth ->
     let prefix =
       sprintf "<!-- host : %s -->@." host in
     stream_contents_with_prefix reqd
       ~prefix ~cache_control:`Public
       ~contents:StaticAssets.(get tryout_aii_html)
  | "/_tscalibs/aii-jslib.js" when getting_method meth ->
     (* let endpoint = sprintf "%s://%s" (Globals.get_client_scheme()) host in *)
     let endpoint = "/" in
     let prefix =
       sprintf "var __tsca_agency_api_endpoint = \"%s\";\n" endpoint in
     stream_contents_with_prefix reqd
       ~prefix ~cache_control:`Public
       (* NB - should change to private caching if access token included *)
       ~contents:StaticAssets.(get aii_jslib_js)
  | _ when getting_method meth && (String.starts_with "/_tscalibs/wouldbe-bil-jslib.js?sprthash=" target) ->
     let sprthash = String.chop_prefix "/_tscalibs/wouldbe-bil-jslib.js?sprthash=" target |> Option.get in
     serve_bil_jslib sprthash reqd
  | _ when getting_method meth -> (
    match calc_static_asset_path() with
    | Some Some asset_path -> serve_static_asset asset_path
    | _ when Option.is_some StaticAssets.webapp_spa_html ->
       stream_contents_with_prefix reqd
         ~cache_control:`Public
         ~contents:StaticAssets.(webapp_spa_html |> Option.get |> get)
    | _ ->
       debug "whatsup? %b" (Option.is_some StaticAssets.webapp_spa_html);
       respond_with_404 ~uri reqd
  )
  | _ -> respond_with_404 ~uri reqd

let rec handle_bookapp_request sprthash reqd =
  let ({ Request.meth; target; _ } as req) = Reqd.request reqd in
  let host = request_host req in
  let uri = (Globals.get_client_scheme())^"://"^
              host^target |> Uri.of_string in
  with_index begin
      fun index ->
      match Hashtbl.find_opt index.sprtindex sprthash with
      | None ->
         (* automatic re-indexing *)
         AgencyIndex.reindex ()
           ~on_finish:(fun index ->
             if not (Hashtbl.mem index.sprtindex sprthash)
             then (respond_with_404 ~uri reqd)
             else handle_bookapp_request sprthash reqd)
           ~on_error:(fun message ->
             respond_with_503 ~uri ~message reqd)
           ~request_desc:(sprintf "access-triggered (%a)" Uri.pp uri)
      | Some sprtid ->
         let spirit = index.spirits.(sprtid) in
         if sprthash <> spirit.idxsprt_sprthash then
           invalid_arg' "panic at %s: %s vs %s" __LOC__ sprthash spirit.idxsprt_sprthash;
         let tmplhash = spirit.idxsprt_tmplhash in
         match target with
         | "/_tscadev/tryout-bil.html" when getting_method meth ->
            let prefix =
              sprintf "<!-- host : %s -->@." host in
            stream_contents_with_prefix reqd
              ~prefix ~cache_control:`Public
              ~contents:StaticAssets.(get tryout_bil_html)
         | "/_tscalibs/bookapp-interface.js" when getting_method meth ->
            serve_bil_jslib sprthash reqd
         | _ ->
            let bookapp_dir = sprintf "%s/%s" Globals.bookapp_repository tmplhash in
            let target_resource = sprintf "%s/%s" bookapp_dir target in
            let default_route_resource = sprintf "%s/%s" bookapp_dir "default.html" in
            match categorize_path target_resource with
            | `File ->
               (* serve the file *)
               stream_contents_with_prefix reqd
                 ~cache_control:`Public
                 ~contents:(
                   AssetCache.get target_resource
                   |> StaticAssets.get)
            | _ ->
               (* serve the default route *)
               stream_contents_with_prefix reqd
                 ~cache_control:`Public
                 ~contents:(
                   AssetCache.get default_route_resource
                   |> StaticAssets.get)
    end

exception ResourceNotFound of string option
exception InvalidRequest of string option
let () =
  Printexc.register_printer (function
      | ResourceNotFound None -> "ResourceNotFound" |> some
      | ResourceNotFound (Some res) -> sprintf "ResourceNotFound(%s)" res |> some
      | InvalidRequest req -> sprintf "InvalidRequest(%a)" (Option.pp pp_string) req |> some
      | _ -> None)

let handle_api_request (path : string) reqd =
  let read = ref [] in
  let reqbody = Reqd.request_body reqd in
  let rec on_read buf ~off ~len =
    refappend read (Bigstringaf.substring buf ~off ~len);
    Body.schedule_read reqbody ~on_read ~on_eof
  and on_eof() =
    let open Sexplib in
    let open Lwt.Infix in
    let reqbody = List.rev !read |> String.concat "" in
    let { Request.meth; target; _ } = Reqd.request reqd in
    let sexp = match meth with
      | `GET -> Std.sexp_of_unit ()
      | _ -> Sexp.of_string reqbody in
    let workload() =
      Bridge.handle_path path sexp >|= fun resp ->
      let resp = Sexp.to_string_mach resp in
      let headers =
        Headers.of_list ([
              "Content-Length", string_of_int (String.length resp)
            ] |> additional_headers ~target) in
      Reqd.respond_with_string reqd (Response.create ~headers `OK) resp in
    let exnfunc = function
        AgencyIndex.AgencyIndexNotAvailable ->
         let msg = "agency index not available" in
         error "500 handling api: %s" msg;
         respond_reqd_with_string ~status:`Internal_server_error reqd msg
      | ResourceNotFound msg ->
         let msg = "Requested resource not found"^(
             match msg with
             | None -> "" | Some m -> ": "^m ) in
         respond_reqd_with_string ~status:`Not_found reqd msg
      | InvalidRequest msg ->
         let msg = "Malformed/Invalid request"^(
             match msg with
             | None -> "" | Some m -> ": "^m ) in
         respond_reqd_with_string ~status:`Bad_request reqd msg
      | exn -> 
         let msg = "Generic server internal error: contact support please" in
         Printexc.(
           error "500 handling api: %s@;%a"
             (to_string exn)
             (List.pp pp_string)
             (get_raw_backtrace() |> raw_backtrace_to_string
              |> String.split_on_char '\n'));
         respond_reqd_with_string ~status:`Internal_server_error reqd msg in
    async_handle ~exnfunc reqd workload in
  Body.schedule_read reqbody ~on_read ~on_eof
  
let pp_sockaddr ppf =
  let open Unix in
  let print fmt = Format.fprintf ppf fmt in
  function
  | ADDR_UNIX path -> print "unix:%s" path
  | ADDR_INET (inet, port) -> print "%s:%d" (string_of_inet_addr inet) port

let request_handler addr reqd =
  let req = Reqd.request reqd in
  access "income request from %a:@;%a"
    pp_sockaddr addr
    Request.pp_hum req;
  let host = request_host req in
  let host = String.split_on_char ':' host |> List.hd in
  let target = req.target in
  match Globals.upstream_mappings ~target with
  | Some (upstream, url) ->
     let remote_ip = match addr with
       | Unix.ADDR_UNIX _ -> None
       | ADDR_INET (inet, _) -> Some (Unix.string_of_inet_addr inet)
     in
     access "relayed to upstream %s => %s" target url;
     handle_upstream ?remote_ip ~upstream ~url reqd
  | None -> begin
      if Apis.recognize_path_invocation req.target |> Option.is_some then (
        handle_api_request req.target reqd
      ) else
        match Globals.parse_bookapp_domain host with
        | None ->
           (* agency route *)
           handle_agency_request reqd
        | Some sprthash ->
           (* bookapp route *)
           handle_bookapp_request sprthash reqd
    end

let error_handler _ ?request:_ error start_response =
  let headers =
    Headers.of_list ([
        ] |> additional_headers) in
  let response_body = start_response headers in
  begin match error with
  | `Exn exn ->
     let exn_str = Printexc.to_string exn in
     Body.write_string response_body
       Printexc.(
       warn "Exception caught: %s" exn_str;
       print_backtrace stderr;
       flush_all();
       exn_str);
     Body.write_string response_body "\n";
  | #Status.standard as error ->
     Body.write_string response_body (Status.default_reason_phrase error)
  end;
  Body.close_writer response_body

(* implementations of the APIs *)
let () =
  let open Aii_dir.AgencyInternalInterface in
  let open Agency_types in
  let chain_id =
    let chain_id, _ =
      Tznode.get_branch ~chain ~block cctxt
      |> strip_tzresult_lwt in
    let tr = Tezos_protocol_011_PtHangz2.Protocol.Environment.Chain_id.to_b58check in
    tr chain_id in
  let supported_network = {
      netident = "testnet";
      chain_id; } in
  let protect_not_found' : 'a 'x . ('a -> string) -> ('a -> 'x) -> ('a -> 'x) =
    fun residfunc func x ->
    try func x with Not_found -> raise (ResourceNotFound (Some (residfunc x))) in
  let [@warning "-26"] protect_not_found func = protect_not_found' identity func in
  let module Books = Available_books in
  let module BookTypes = Book_intf.BookTypes in

  Bridge.register_handler
    TezosUtils.CalculateAddressFromLedgerPublicKey (fun hex ->
      let open Tezos_stdlib in
      let open Tezos_crypto in
      let bytes = `Hex hex |> Hex.to_bytes in
      TzEndian.set_int8 bytes 0 0 ;
      let pk = Data_encoding.Binary.of_bytes_exn Signature.Public_key.encoding bytes in
      let pkh = Signature.Public_key.hash pk in
      Signature.Public_key_hash.to_b58check pkh |> Lwt.return);

  Bridge.register_handler
    RefMaster.DefaultTezosNetwork (fun () ->
      supported_network |> Lwt.return
    );

  Bridge.register_handler
    RefMaster.ListAvailableTezosNetwork (fun () ->
      let networks = [ supported_network ] in
      networks |> Lwt.return
    );

  Bridge.register_handler
    RefMaster.ListAdvertizedBooks (fun () ->
      Books.advertized_books |&> (fun entry -> {
        bookident = entry.bookident;
        bookhash = entry.bookhash;
        title = entry.title;
        synopsis = entry.synopsis;
      }) |> Lwt.return
    );

  Bridge.register_handler
    RefMaster.ListBooksForTemplate (fun tmplhash ->
      Books.books_for_template tmplhash
      |&> (fun bk -> bk.entry.bookhash)
      |> Lwt.return
    );

  Bridge.register_handler
    RefMaster.GetBookStatus ((fun bookhash ->
      let book = Books.available_books |> List.assoc bookhash in
      let { BookTypes.entry = { bookident; _ };
            agency_charge; provider_charge;
            contract_complexity;
            certification_status; } = book in
      let charges = { agency_charge; provider_charge } in
      let review_results = { contract_complexity; certification_status; } in
      { bookident; bookhash; charges; review_results }
      |> Lwt.return
    ) |> protect_not_found' (sprintf "bookhash = %s"));

  Bridge.register_handler
    RefMaster.GetProviderInfo ((fun providerindent ->
      Books.all_providers |> List.assoc providerindent |> Lwt.return
    ) |> protect_not_found' (sprintf "providerident = %s"));

  Bridge.register_handler
    InfoBank.GetBookEntry ((fun bookhash ->
      let book = Books.available_books |> List.assoc bookhash in
      let { BookTypes.entry; _ } = book in
      entry |> Lwt.return
    ) |> protect_not_found' (sprintf "bookhash = %s"));

  Bridge.register_handler
    IndexerLevel0.GetSpiritInfo ((fun sprthash ->
      with_index (fun index ->
          match AgencyIndex.lookup_spirit index ~sprthash with
          | None -> raise Not_found
          | Some spirit ->
             { sprthash;
               network = supported_network; (* XXX *)
               tmplhash = spirit.idxsprt_tmplhash;
               broker = spirit.idxsprt_broker;
               requester = spirit.idxsprt_requester;
               ensemble  = spirit.idxsprt_ensemble; }
        ) |> Lwt.return) |> protect_not_found' (sprintf "no such spirit with sprthash = %s"));

  Bridge.register_handler
    IndexerLevel0.GetSpiritStatus ((fun sprthash ->
      with_index (fun idx ->
          let open AgencyIndex in
          match lookup_spirit_with_avatars idx ~sprthash with
          | None -> raise Not_found
          | Some (spirit, avatars) ->
             let info = {
               sprthash;
               network = supported_network; (* XXX *)
               tmplhash = spirit.idxsprt_tmplhash;
               broker = spirit.idxsprt_broker;
               requester = spirit.idxsprt_requester;
               ensemble  = spirit.idxsprt_ensemble;
             } in
             let avatars =
               let tr av =
                 IndexerLevel0.{
                     balance = av.idxav_balance;
                     wstore = av.idxav_wstore |> Commons.hex_of_bytes;
                     wstore_unpacked = av.idxav_wstore_unpacked
                 } in
               avatars |&> (fun (rclabel, av) -> rclabel, tr av) in
             IndexerLevel0.{
                 spirit = info;
                 avatars;
             }
        ) |> Lwt.return) |> protect_not_found' (sprintf "no such spirit with sprthash = %s"));

  let genBcdHyperlink kind _network hash =
    let prefix = "https://better-call.dev/delphinet" in
    let url = match kind with
    | `Contract -> sprintf "%s/%s/storage" prefix hash
    | `Operation -> sprintf "%s/opg/%s" prefix hash in
    let title = "Explorer Link (Better Call Dev)" in
    { title; synopsis = Some title; url } in

  let genTzstatsHyperlink kind _network hash =
    let prefix = "https://delphi.tzstats.com" in
    let url = match kind with
    | `Contract -> sprintf "%s/%s" prefix hash
    | `Operation -> sprintf "%s/%s" prefix hash in
    let title = "Explorer Link (TzStats)" in
    { title; synopsis = Some title; url } in

  Bridge.register_handler
    Proto0.ExplorerLinksForContract (fun (network, hash) ->
      let kind = `Contract in
      [ genTzstatsHyperlink kind network hash;
        genBcdHyperlink kind network hash
      ] |> Lwt.return
    );

  Bridge.register_handler
    Proto0.ExplorerLinksForOperation (fun (network, hash) ->
      let kind = `Operation in
      [ genTzstatsHyperlink kind network hash;
        genBcdHyperlink kind network hash
      ] |> Lwt.return
    );

  Bridge.register_handler
    Proto0.BookAppUrlForSpirit (fun sprthash ->
      let open Globals in
      let url = sprintf "%s://%s.%s:%d" (get_client_scheme())
                  sprthash bookapp_base_domain Globals.bookapp_port in
      { synopsis = Some ("BookApp URL for "^sprthash);
        title = "BookApp URL";
        url; } |> Lwt.return
    );

  let find_tmplversion ?tmplversion tmplhash =
    let open Book_intf.TmplversionTypes in
    let versions = Books.available_tmplversions in
    match tmplversion with
    | Some tmplversion ->
       ((fun v -> List.find (fun e -> e.tmplversion = v)
                    (versions |> List.fmap snd))
        |> protect_not_found' (sprintf "tmplversion = %s"))
         tmplversion
    | None ->
       ((fun h -> match List.assoc h versions with
                  | [] -> raise Not_found
                  | h :: _ -> h)
        |> protect_not_found' (sprintf "tmplversion with tmplhash = %s"))
         tmplhash in

  let booksdk = Booksdk.ocaml_booksdk in

  let strip_aresult = function
    | Ok x -> x
    | Error msg ->
       raise (InvalidRequest (Some msg)) in

  let check_network net =
    if net <> supported_network
    then
      let pp_net = Sexplib.Sexp.pp_mach %% sexp_of_tezos_network in
      let msg =
        sprintf "network: %a; expecting %a"
          pp_net net pp_net supported_network in
        raise (ResourceNotFound (Some (msg))) in

  Bridge.register_handler
    Proto0.ProcessGenesisRequest (fun req ->
      check_network req.network;
      let tmplhash = req.template in
      let tmplversion = find_tmplversion tmplhash in
      let broker, tmplid = with_index (fun idx ->
          match AgencyIndex.list_templates
                  ~availability_filter:true
                  ~tmplhash_filter:tmplhash idx with
          | [] -> raise
               (ResourceNotFound (Some ("broker with template where tmplhash = : "^tmplhash)))
          | h :: _ -> h.idxtmpl_broker, h.idxtmpl_tmplid) in
      let book = match Books.books_for_template tmplhash with
        | [] -> raise (ResourceNotFound (Some ("book with tmplhash = : "^tmplhash)))
        | h :: _ -> h in
      tmplversion.genspell_interpreter booksdk
        ~spell:req.spell >|= strip_aresult >>= fun (genparam_bytes, initbal) ->
      let genparam = Commons.hex_of_bytes genparam_bytes in
      let sprthash =
        let identstr =
          sprintf "sprthash by agency_server(%s) at %.3f with request %a"
            (Globals.global_nounce())
            (Unix.gettimeofday())
            pp_sexp (Proto0.sexp_of_genesis_request req) in
        Tzcrypto.(calc_sprthash ~testnet:true identstr) in
      let { BookTypes.agency_charge; provider_charge; _ } = book in
      let raw_tmplhash = Tzcrypto.(
          b58check_decode Arbitrarity.tmplhash_scheme tmplhash
          |> Option.get |> Bytes.of_string) in
      (* let open Brokerlib in *)
      info "agency/provider_charge : %Ld / %Ld" agency_charge provider_charge;
      let open Broker_models.BrokerDataModels in
      let open Bridgelib.Bridge_commands in
      let broker_message : broker_message_data =
        let to_bytes x = `bytes (Bytes.to_string x) in
        `GenesisRequest (
            `sudo false,
            `genreq (
                (`sprthash sprthash),
                (`tmplid (`nat tmplid)),
                (`tmplhash (raw_tmplhash |> to_bytes)),
                (`genparam (genparam_bytes |> to_bytes)),
                (`initbalance (`mutez initbal)),
  
                (`broker_fee (`mutez agency_charge)),
                (`provider_fee (`mutez provider_charge))
              )
          ) in
      let txn_argument =
        BrokerlibBridge.to_michelson Broker_message_data broker_message
        |-> Result.iter_error
              (error "BrokerlibBridge.to_michelson Broker_message_data - %a"
                 pp_exn)
        |>  Result.get_ok in
      let txn_amount =
        let (+) = Int64.add in
        agency_charge + provider_charge + initbal in
      let cli_instructions = Proto0.[
          InstructionComment "you may be asked to include an --burn-cap parameter";
          InstructionParameter ("tezos_client", "path to the official tezos-client binary");
          TezosClientCommand [
              true, "tezos_client";
              false, "transfer";
              false, (Q.(div (of_int64 txn_amount)
                           (of_int 1000_000))
                      |> Q.to_float |> sprintf "%g");
              false, "from";
              false, req.requester;
              false, "to";
              false, broker;
              false, "-q";
              false, "--arg";
              false, txn_argument;
            ]
        ] in
      {
        Proto0.sprthash;
        initbal;
        agency_charge;
        provider_charge;
        cli_instructions;
        broker;
        tmplid;
        genparam;

        txn_argument;
        txn_amount;
      } |> Lwt.return
    );

  Bridge.register_handler
    Proto0.ProcessInvocationRequest (fun req ->
      check_network req.network;
      let sprthash = req.spirit in
      let tmplhash, ensemble = with_index (fun idx ->
          match AgencyIndex.lookup_spirit ~sprthash idx with
          | None -> raise
               (ResourceNotFound (Some ("spirit with sprthash = : "^sprthash)))
          | Some entry ->
             entry.idxsprt_tmplhash, entry.idxsprt_ensemble) in
      let tmplversion = find_tmplversion tmplhash in
      (tmplversion.invspell_interpreter booksdk ~spell:req.spell
       >|= strip_aresult) >>= fun (rclabel, wparam, amount) ->
      let ktaddr = List.assoc rclabel ensemble in (* XXX *)
      let open Commons in
      let invparam = hex_of_bytes wparam in
      let txn_argument = prefixed_hex_of_bytes wparam in
      Tzutils.Michelson.unpack_data wparam >>= fun invparam_unpacked ->
      let invparam_unpacked =
        invparam_unpacked |> Result.to_option
        |> Option.map Tzutils.Michelson.string_of_expr in
      let cli_instructions = Proto0.[
          InstructionComment "you may be asked to include an --burn-cap parameter";
          InstructionParameter ("tezos_client", "path to the official tezos-client binary");
          TezosClientCommand [
              true, "tezos_client";
              false, "transfer";
              false, (Q.(div (of_int64 amount)
                           (of_int 1000_000))
                      |> Q.to_float |> sprintf "%g");
              false, "from";
              false, req.requester;
              false, "to";
              false, ktaddr;
              false, "-q";
              false, "--arg";
              false, txn_argument;
            ]
        ] in
      {
        sprthash;
        Proto0.invparam;
        invparam_unpacked;
        amount = amount;
        dest_rclabel = rclabel;
        dest_ktaddr = ktaddr;
        cli_instructions;

        txn_argument;
        txn_amount = amount;
      } |> Lwt.return
    );

  Bridge.register_handler
    Proto0.InterpretSpiritStatus (fun req ->
      let open Proto0 in
      let open Book_intf.TmplversionTypes in
      let { sprthash; tmplversion; silabel } = req in
      let tmplhash, spirit = with_index (fun idx ->
          match AgencyIndex.lookup_spirit_with_avatars idx ~sprthash with
          | None -> raise
               (ResourceNotFound (Some ("spirit with sprthash = : "^sprthash)))
          | Some (spirit, avatars) ->
             let tmplhash = spirit.idxsprt_tmplhash in
             let avatars =
               avatars |&> (fun (rclabel, av) ->
                 { rclabel;
                   address = av.idxav_address;
                   balance = av.idxav_balance;
                   wstore = av.idxav_wstore;
                   wstore_unpacked = av.idxav_wstore_unpacked}
               ) in
             tmplhash,
             { sprthash;
               tmplhash;
               broker = spirit.idxsprt_broker;
               requester = spirit.idxsprt_requester;
               avatars;
             }) in
      let tmplversion = find_tmplversion ?tmplversion tmplhash in
      (match List.find_opt (fun e -> e.silabel = silabel)
               tmplversion.spirit_interpreters with
       | None -> Lwt.return Proto0.NoSuchSpiritInterpreter
       | Some intrp ->
          (intrp.interpreter booksdk) spirit >|= function
          | Ok x -> InterpretationResult x
          | Error message -> InterpretationError { message }));

  Bridge.register_handler
    Proto0.DescribeSpellAssistant (fun req ->
      let open Proto0 in
      let open Book_intf.TmplversionTypes in
      let (tmplversion, salabel) = req in
      let tmplversion = List.find (fun x -> x.tmplversion = tmplversion)
                          (Books.available_tmplversions |&> snd |> List.flatten) in
      let sa = List.find (fun e -> e.salabel = salabel)
                 tmplversion.spell_assistants in
      let desc = {
          salabel;
          tmplversion = tmplversion.tmplversion;
          form_title = sa.form_title;
          form_desc = sa.form_desc;
          form_major_button = sa.form_major_button;
          form_fields = sa.form_fields;
        } in
      Lwt.return desc);

  Bridge.register_handler
    Proto0.SpellAssistantInterpret (fun req ->
      let open Proto0 in
      let open Book_intf.TmplversionTypes in
      let { salabel; tmplversion; filled_form } = req in
      let tmplversion = List.find (fun x -> x.tmplversion = tmplversion)
                          (Books.available_tmplversions |&> snd |> List.flatten) in
      let sa = List.find (fun e -> e.salabel = salabel)
                 tmplversion.spell_assistants in
      (match sa.form_interpreter booksdk filled_form with
       | Ok spell -> SuccessfulSpellAssistantInterpretation spell
       | Error (error_message, hints) ->
          UnsuccessfulSpellAssistantInterpretation {
              error_message; hints;
            }
       | exception e ->
          InvalidSpellAssistantInput {
              error_message = Some (Printexc.to_string e)
      }) |> Lwt.return);

  let mutez mutez =
    let module Tez = Tznode.Alpha_context.Tez in
    mutez |> Tez.of_mutez |> Option.get in

  Bridge.register_handler
    Proto0.SimulateGenesis (fun (req, proc) ->
      Tznode.simulate cctxt ~chain ~block
        ~source:req.requester ~destination:proc.broker
        ~arg:proc.txn_argument
        ~amount:(mutez proc.txn_amount)
        () >|= function
      | `Failed error_message -> Proto0.SimulationFailed { error_message }
      | `Succeeded (logs, _oph, (watermark, unsigned), _, _, _) ->
         let simulation_output =
           logs |&> (fun (_, msg) -> msg) |> String.concat "\n" in
         let open Commons in
         SimulationSucceeded {
             unsigned_transaction = hex_of_bytes unsigned;
             watermark = hex_of_bytes watermark;
             simulation_output;
           }
    );

  Bridge.register_handler
    Proto0.SimulateInvocation (fun (req, proc) ->
      Tznode.simulate cctxt ~chain ~block
        ~source:req.requester ~destination:proc.dest_ktaddr
        ~arg:proc.txn_argument
        ~amount:(mutez proc.amount)
        () >|= function
      | `Failed error_message -> Proto0.SimulationFailed { error_message }
      | `Succeeded (logs, _oph, (watermark, unsigned), _, _, _) ->
         let simulation_output =
           logs |&> (fun (_, msg) -> msg) |> String.concat "\n" in
         let open Commons in
         SimulationSucceeded {
             unsigned_transaction = hex_of_bytes unsigned;
             watermark = hex_of_bytes watermark;
             simulation_output;
           }
    );

  Bridge.register_handler
    Proto0.InjectOperation (fun req ->
      let open Proto0 in
      let open Tzutils in
      let { network;
            unsigned_transaction;
            signature;
          } = req in
      if network <> supported_network
      then Lwt.fail_invalid_arg "network not supported"
      else (
        let unsigned, signature =
          bytes_of_hex unsigned_transaction,
          `HexOrBase58check signature in
        Tznode.inject ~chain ~block cctxt ~unsigned ~signature () >|= function
        | Error err ->
           let error_message =
             sprintf "%a"
               Tezos_error_monad.Error_monad.pp_print_trace
               err in
           InjectionFailed { error_message }
        | Ok (ophash, originated_contracts) ->
           let on_finish index = 
             debug "re-index done. %s" (index_info (Some index)) in
           let on_error msg =
             error "%s" msg in
           let request_desc = sprintf "injection triggered auto-reindex" in
           AgencyIndex.reindex ~on_finish ~on_error ~request_desc();
           InjectionSucceeded {
               ophash;
               originated_contracts
             }));
  ()

(* process entrypoint *)

let main() =
  let open Kxclib.ArgOptions in
  let bind_addr, bind_desc = match get_option (StringOption "-bind") with
    | None -> Unix.inet_addr_loopback, "loopback"
    | Some addr ->
       Unix.inet_addr_of_string addr, addr in
  let port = Globals.server_port in
  let listen_address = Unix.(ADDR_INET (bind_addr, port)) in
  let server_mode_desc =
    if has_flag "-dev-ssl"
    then "Dev-HTTPS"
    else "HTTP" in
  let mkserver () sock conn =
    if has_flag "-dev-ssl" then (
      let certfile, keyfile =
        get_option_exn (StringOption "-sslcert"),
        get_option_exn (StringOption "-sslkey") in
      Lwt.catch (fun () ->
          Server.TLS.create_connection_handler
            ~certfile ~keyfile
            ~request_handler
            ~error_handler
            sock conn)
        Printexc.(fun exn ->
        error "exception caught via Lwt.catch: %s@;%s"
          (to_string exn)
          (get_raw_backtrace() |> raw_backtrace_to_string);
        Lwt.return_unit)
    ) else (
      Server.create_connection_handler
        ~request_handler
        ~error_handler
        sock conn
    ) in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket
        listen_address (mkserver())
      >|= fun _ ->
      Globals.client_scheme := Some "https";
      info "TSCA Agency server started in %s mode, listening on port %d at %s"
        server_mode_desc port bind_desc);
  (match ArgOptions.(get_option (FloatOption "-reindex-per-nsec")) with
   | Some nsec -> Lwt.async (
     let rec loop() =
       Lwt_unix.sleep nsec >>= fun () ->
       AgencyIndex.reindex ~request_desc:"periodic" ();
       loop()
     in loop)
   | _ -> ()
  );
  let forever, _ =
    (if ArgOptions.has_flag "-reindex-at-start"
     then
       info "re-indexing";
       AgencyIndex.reindex());
    Lwt.wait () in
  Lwt_main.run forever

let () = main()
