type 'x lwt = 'x Lwt.t
type http_method = [ `Get | `Put | `Post ]

module Err = struct
  type 'x t = ('x, string) result lwt
  let pure x = Lwt.return (ResultWithErrmsg.pure x)
  let bind : 'x t -> ('x -> 'y t) -> 'y t =
    fun ma f -> Lwt.Infix.(
      ma >>= function
      | Error msg -> Lwt.return (Error msg)
      | Ok t -> f t)
  include MonadOps(struct
              type nonrec 'x t = 'x t
              let pure = pure
              let bind = bind
            end)
end
type 'x aresult = 'x Err.t

module type HttpClient = sig
  type 'x http_result
  val perform_request :
    url:string ->
    params:(string*string) list ->
    headers:(string*string) list ->
    meth:http_method ->
    body:string ->
    string http_result
  val map_http_result :
    ('x -> 'y) -> 'x http_result -> 'y http_result
end

module type EmailBackend = sig
  type config
  val send_email :
    config:config ->

    from:string ->

    recipients:string list ->
    ?cc:string list ->
    ?bcc:string list ->

    subject:string ->
    body:[ `TextBody of string | `HtmlBody of string ]
    -> string aresult (** returning the Message-ID of the email sent if successful *)
end

type email_backend = (module EmailBackend)
