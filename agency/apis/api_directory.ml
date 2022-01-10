type uri = Uri.t
type sexp = Sexplib.Sexp.t

module Sexp_converters = struct
  open Sexplib.Std
  let sexp_of_uri uri =
    uri |> Uri.to_string |> sexp_of_string
  let uri_of_sexp sexp =
    sexp |> string_of_sexp |> Uri.of_string
end

module Auxtypes = struct
  open Sexplib.Std

  type http_method = [ `Get | `Put | `Post ]
  [@@deriving sexp]
  (** limited to methods that we plan to use *)

  type 'wireformat http_request_descriptor = {
      hreq_url : string;
      hreq_meth : http_method;
      hreq_body : 'wireformat;
      hreq_headers : (string*string) list;
      (** it is possible to have plural headers *)
    }
  [@@deriving sexp]

  type 'x sexp_enc = ('x -> sexp)*(sexp -> 'x)

  module Http_endpoint = struct
    (* context will be normalized
     * (1) to lack trailing slash, and
     * (2) to have leading slash
     * 
     * empty context will be represented as an empty string,
     * which means rule (1) takes precedence *)
    type t =
      | FullEndpoint of {
          protocol : [ `Http | `Https ];
          host : string;
          port : int;
          context : string;
        }
      | RelativeEndpointSameProtocol of {
          host : string;
          port : int option;
          context : string;
        }
      | RelativeEndpointSameAuthority of {
          context : string;
        }

    (* we allow 2 forms of FullEndpoint strings
     * - 'scheme://host[:port]/context'
     * - 'host[:port]/context' => default scheme to `Http
     * 
     * and also 2 forms of RelativeEndpoint* strings
     * - '/context'
     * - '//host[:port]/context' *)
    let parse_endpoint str =
      let parse_after_scheme rest =
        (* rest must have form 'host[:port]/context', where '/context' could be empty *)
        let len = String.length rest in
        let first_colon = String.index_opt rest ':' in
        let first_slash = String.index_opt rest '/' in
        let host_e, port_e, context_s =
          match first_slash, first_colon with
          | None, None -> len, len, len
          | Some s, None -> s, s, s
          | None, Some c -> c, len, len
          | Some s, Some c when c < s -> c, s, s
          | Some s, Some _ -> s, s, s in
        let sub s e = String.sub rest s (e-s) in
        let host = sub 0 host_e in
        let port = if host_e = port_e then None
                   else sub (succ host_e) port_e |> int_of_string |> Option.some in
        let context =
          let components =
            sub context_s len |> String.split_on_char '/'
            |> List.filter ((<>) "") in
          match components with
          | [] -> ""
          | many -> "/"^(String.concat "/" many) in
        host, port, context
        in
      if String.starts_with "//" str then (
        (* must be of form '//host[:port]/context' *)
        let host, port, context =
          String.chop_prefix "//" str |> Option.get |> parse_after_scheme in
        RelativeEndpointSameProtocol {
            host; port; context;
          }
      ) else if String.starts_with "/" str then (
        (* must be of form '/context' *)
        RelativeEndpointSameAuthority {
          context = String.chop_prefix "/" str |> Option.get
          }
      ) else match Str.(split (regexp "://")) str with
        | [rest] ->
           (* must be of form 'host[:port]/context' *)
           let host, port, context =
             parse_after_scheme rest in
           let port = port |> Option.v 80 in
           FullEndpoint {
                host; port; protocol = `Http;
               context;
             }
        | ["http" as scheme; rest] | ["https" as scheme; rest] ->
           (* must be of form 'scheme://host[:port]/context' *)
           let protocol = match scheme with
             | "http" -> `Http | "https" -> `Https | _ -> failwith "panic" in
           let host, port, context =
             parse_after_scheme rest in
           let port = match port, protocol with
             | None, `Http -> 80
             | None, `Https -> 443
             | Some p, _ -> p in
           FullEndpoint {
               host; port; protocol;
               context;
             }
        | _ -> invalid_arg ("malformed endpoint: "^str)
         
    let http_endpoint ~tls host port context =
      let protocol = if tls then `Https else `Http in
      FullEndpoint { host; port; protocol; context }

    let relative_endpoint = function
      | `SameAuthority context ->
         RelativeEndpointSameAuthority { context }
      | `SameProtocol (host, port, context) ->
          RelativeEndpointSameProtocol { host; port; context }

    let uri_of_path ?path:(path="/") ?require_absolute:(absolute=false) endp =
      match absolute, endp with
      | true, (FullEndpoint endp) | false, (FullEndpoint endp) ->
         let protocol, port = endp.protocol, endp.port in
         let scheme = match protocol with `Http -> "http" | `Https -> "https" in
         let port_str = match protocol, port with
           | `Http, 80 -> "" | `Https, 443 -> "" | _ -> ":"^(string_of_int port) in
         Format.sprintf "%s://%s%s%s" scheme endp.host port_str (endp.context^path)
      | true, _ -> invalid_arg "full endpoint required"
      | _, RelativeEndpointSameProtocol { host; port; context } ->
         let port_str = Option.(port |> map (fun p -> ":"^(string_of_int p)) |> v "") in
         "//"^host^port_str^context^path
      | _, RelativeEndpointSameAuthority { context } ->
         context^path

    let http_example = parse_endpoint "http://example.com"
  end

  type http_endpoint = Http_endpoint.t
end open Auxtypes

module type ApiDirectory = sig
  type invp (** invocation point (for specific individual api) *)
  type ('reqty, 'respty) typed_invp
  (** typed invocation point *)

  val untyped : _ typed_invp -> invp

  (** query the validness of an invocation-point within this directory *)

  val valid_typed : _ typed_invp -> bool
  val valid_untyped : invp -> bool

  val sample_url_of_invp : ?endpoint:http_endpoint -> invp -> string
  val name_of_invp : invp -> string
  val desc_of_invp : invp -> string

  val wireformat_sexp :
    ('reqty, 'respty) typed_invp ->
    ('reqty sexp_enc) * ('respty sexp_enc)

  val prepare_http_invocation_sexp :
    endpoint:http_endpoint ->
    ('reqty, 'respty) typed_invp ->
    'reqty ->
    sexp http_request_descriptor
    * (sexp -> 'respty)

  val recognize_path_invocation : string -> invp option
end

module ApiClient
         (Dir : ApiDirectory)
         (HttpClient : sig
             type 'x http_result
             val perform_request :
                    url:string ->
                    headers:(string*string) list ->
                    meth:http_method ->
                    body:string ->
                    string http_result
             val map_http_result :
               ('x -> 'y) -> 'x http_result -> 'y http_result
           end)
         (HttpConfig : sig
            val endpoint : http_endpoint
          end) = struct
  type 'x http_result = 'x HttpClient.http_result
  type ('reqty, 'respty) typed_invp =
    ('reqty, 'respty) Dir.typed_invp
  let invoke (type reqty) (type respty) :
    (reqty, respty) typed_invp ->
    reqty -> respty http_result =
    fun  invp req ->
    let open Sexplib in
    let open HttpConfig in
    let reqd, respenc =
      Dir.prepare_http_invocation_sexp ~endpoint
        invp req in
    let body = Sexp.to_string_mach reqd.hreq_body in
    HttpClient.(
      perform_request
        ~url:reqd.hreq_url
        ~headers:reqd.hreq_headers
        ~meth:reqd.hreq_meth
        ~body
      |> map_http_result (respenc % Sexp.of_string))
end

module ApiHandlerBridge
         (Dir : ApiDirectory)
         (M : sig
            type 'x t
            val pure : 'x -> 'x t
            val bind : 'x t -> ('x -> 'y t) -> 'y t
          end)
= struct
  type 'x m = 'x M.t

  type invp = Dir.invp
  type ('reqty, 'respty) typed_invp =
    ('reqty, 'respty) Dir.typed_invp

  type handler =
    | Handler :
        ('reqty, 'respty) typed_invp *
          ('reqty -> 'respty m)
        -> handler

  let registry = Hashtbl.create 0

  let register_handler (type reqty) (type respty) :
     (reqty, respty) typed_invp ->
     (reqty -> respty m) -> unit =
    fun invp func ->
    Hashtbl.replace registry
      (Dir.untyped invp)
      (Handler (invp, func))

  let handle : invp -> sexp -> sexp m =
    fun invp reqbody ->
    let open MonadOps(M) in
    match Hashtbl.find_opt registry invp with
    | None ->
       invalid_arg
         "no handler registered for the requested api"
    | Some (Handler (invp, handler)) ->
       let req_enc, resp_enc = Dir.wireformat_sexp invp in
       let req = (snd req_enc) reqbody in
       handler req >|= (fst resp_enc)

  exception Unrecognized_route of string
  let handle_path : string -> sexp -> sexp m =
    fun path reqbody ->
    match Dir.recognize_path_invocation path with
    | None -> raise (Unrecognized_route path)
    | Some invp -> handle invp reqbody
end

module ApiRegistarType(Conf : sig
             type ('reqty, 'respty) typed_invp
           end) = struct
  open Conf
  type api_registrar =
    < register_generic :
        'reqty 'respty .
        ('reqty, 'respty) typed_invp ->
        urlpath:string ->
        name:string ->
        desc:string ->
        methfunc:('reqty -> http_method) ->
        reqenc:('reqty sexp_enc) ->
        respenc:('respty sexp_enc) ->
        unit;
      register_get :
        'respty .
        (unit, 'respty) typed_invp ->
        urlpath:string ->
        name:string ->
        desc:string ->
        respenc:('respty sexp_enc) ->
        unit;
      register_post :
        'reqty 'respty .
        ('reqty, 'respty) typed_invp ->
        urlpath:string ->
        name:string ->
        desc:string ->
        reqenc:('reqty sexp_enc) ->
        respenc:('respty sexp_enc) ->
        unit;
    >
end

module SimpleApiDirectory(Conf : sig
             type ('reqty, 'respty) typed_invp
             include module type of
               ApiRegistarType(struct
                   type nonrec ('reqty, 'respty) typed_invp =
                     ('reqty, 'respty) typed_invp
                 end)
             val register_all : api_registrar -> unit
           end)
       : ApiDirectory
       with type ('reqty, 'respty) typed_invp =
                   ('reqty, 'respty) Conf.typed_invp
= struct
  type ('reqty, 'respty) typed_invp =
    ('reqty, 'respty) Conf.typed_invp
  type invp = Untyped : _ typed_invp -> invp

  type ('reqty, 'respty) registry_entry = {
      urlpath : string;
      name : string;
      desc : string;
      methfunc : 'reqty -> http_method;
      reqenc   : 'reqty sexp_enc;
      respenc  : 'respty sexp_enc;
    }
  type any_registry_entry =
    Registry_entry :
      (('reqty, 'respty) typed_invp
       * ('reqty, 'respty) registry_entry) ->
      any_registry_entry

  let untyped invp = Untyped invp

  let registry : (invp, any_registry_entry) Hashtbl.t = Hashtbl.create 0
  let path_index = ref None

  let lookup_info invp =
    let open MonadOps(Option) in
    Hashtbl.find_opt registry invp >|=
      fun (Registry_entry
             (_, { urlpath; name; desc; _ })) ->
      (urlpath, name, desc)

  let lookup_typed (type reqty) (type respty) :
        (reqty, respty) typed_invp ->
        (reqty, respty) registry_entry option =
    fun invp ->
    let open MonadOps(Option) in
    Hashtbl.find_opt registry (Untyped invp) >|=
      fun (Registry_entry
             (_, entry)) ->
      entry |> Obj.magic
      (** educated playing with fire here :D *)

  let valid_untyped invp = Hashtbl.mem registry invp
  let valid_typed invp = valid_untyped (untyped invp)

  let sample_url_of_invp
        ?endpoint:(endp=Http_endpoint.http_example) invp =
    lookup_info invp |> Option.get
    |> fun (path,_,_) ->
       Http_endpoint.uri_of_path ~path endp

  let name_of_invp invp =
    lookup_info invp |> Option.get
    |> fun (_,name,_) -> name

  let desc_of_invp invp =
    lookup_info invp |> Option.get
    |> fun (_,_,desc) -> desc

  let recognize_path_invocation path =
    Hashtbl.find_opt (Option.get !path_index) path

  let wireformat_sexp (type reqty) (type respty) :
        (reqty, respty) typed_invp ->
        (reqty sexp_enc) * (respty sexp_enc) =
    fun invp ->
    let open MonadOps(Option) in
    (lookup_typed invp >|= fun { reqenc; respenc; _ } ->
     (reqenc, respenc)) |> Option.get

  let prepare_http_invocation_sexp
        ~endpoint (type reqty) (type respty) :
        (reqty, respty) typed_invp ->
        reqty ->
        sexp http_request_descriptor
        * (sexp -> respty) =
    fun invp req ->
    let e = lookup_typed invp |> Option.get in
    let url = Http_endpoint.uri_of_path ~path:e.urlpath endpoint in
    let meth = e.methfunc req in
    let req = (fst e.reqenc) req in
    let reqd = {
        hreq_url = url;
        hreq_meth = meth;
        hreq_body = req;
        hreq_headers = [];
      } in
    reqd, (snd e.respenc)

  let () =
    let open Sexplib.Std in
    let intf = object (self)
        method register_generic :
                   'reqty 'respty .
                    ('reqty, 'respty) typed_invp ->
                    urlpath:string ->
                    name:string ->
                    desc:string ->
                    methfunc:('reqty -> http_method) ->
                    reqenc:('reqty sexp_enc) ->
                    respenc:('respty sexp_enc) ->
                    unit = fun
            invp ~urlpath ~name ~desc
            ~methfunc ~reqenc ~respenc ->
          let entry = {
              urlpath; name; desc;
              methfunc; reqenc; respenc
            } in
          let entry = Registry_entry (invp, entry) in
          Hashtbl.replace registry (Untyped invp) entry
        method register_get :
                    'respty .
                    (unit, 'respty) typed_invp ->
                    urlpath:string ->
                    name:string ->
                    desc:string ->
                    respenc:('respty sexp_enc) ->
                    unit = fun
            invp ~urlpath ~name ~desc
            ~respenc ->
          self#register_generic
            invp ~urlpath ~name ~desc
            ~reqenc:(sexp_of_unit, unit_of_sexp)
            ~respenc
            ~methfunc:(constant `Get)
        method register_post :
                   'reqty 'respty .
                    ('reqty, 'respty) typed_invp ->
                    urlpath:string ->
                    name:string ->
                    desc:string ->
                    reqenc:('reqty sexp_enc) ->
                    respenc:('respty sexp_enc) ->
                    unit = fun
            invp ~urlpath ~name ~desc
             ~reqenc ~respenc ->
          self#register_generic
            invp ~urlpath ~name ~desc
            ~reqenc ~respenc
            ~methfunc:(constant `Post)
      end in
    Conf.register_all intf;
    path_index := Some (Hashtbl.create (Hashtbl.length registry));
    Hashtbl.iter (fun invp (Registry_entry (_, { urlpath; _ })) ->
        Hashtbl.replace (Option.get !path_index) urlpath invp
      ) registry
end
