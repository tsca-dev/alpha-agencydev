module type S = sig
  type 'x t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val pure : 'a -> 'a t
  val inject_error : string -> 'a t
  val inject_error' : exn -> 'a t
  val check_io : 'x t -> ('x, string) result t
end
type io_style = (module S)

module DirectIoStyle = struct
  type 'x t = ('x, Printexc.t) result
  let bind x f =
    try Result.bind x f with e -> Error e
  let pure x : 'x t = Ok x
  let inject_error' : exn -> 'a t =
    fun e -> Error e
  let inject_error err = inject_error' (Failure err)
  let check_io : 'x t -> ('x, string) result t = function
    | Ok x -> pure (Ok x)
    | Error x -> pure (Error (Printexc.to_string x))
end
module CheckDirectIoStyle : S = DirectIoStyle
