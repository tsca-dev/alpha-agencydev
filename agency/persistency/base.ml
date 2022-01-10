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

module type StorageBackend = sig
  type store
  type config

  val open_store : config -> store
  val close_store : store -> unit aresult

  val max_key_length : int
  val max_object_length : int

  val has_object :
    store:store -> key:string
    -> bool aresult
  val get_object_opt :
    store:store -> key:string
    -> string option aresult
  val get_object_err :
    store:store -> key:string
    -> string aresult
  val put_object :
    store:store -> key:string
    -> string -> unit aresult
  val exchange_object :
    store:store -> key:string
    -> string -> string option aresult
  (** returning the old object,
      or None if no object associate with the given key *)

  val delete_object :
    store:store -> key:string
    -> unit aresult
  (** error if no object associated with the given key exists *)

end

type storage_backend = (module StorageBackend)
