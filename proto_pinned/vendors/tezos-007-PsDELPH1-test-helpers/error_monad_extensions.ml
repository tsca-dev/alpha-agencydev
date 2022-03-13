open Error_monad

let ( >>? ) v f = match v with Error _ as err -> err | Ok v -> f v

let ( >>=? ) v f =
  v >>= function Error _ as err -> Lwt.return err | Ok v -> f v

let ( >>?= ) v f = match v with Error _ as e -> Lwt.return e | Ok v -> f v

let ( >|?= ) v f =
  match v with Error _ as e -> Lwt.return e | Ok v -> f v >>= Lwt.return_ok

let ( >|=? ) v f = v >>=? fun v -> Lwt.return_ok (f v)

let ( >|= ) = Lwt.( >|= )

let ( >|? ) v f = v >>? fun v -> Ok (f v)

module Option = struct
  include Option
  let value = Stdlib.Option.value
  let get = Stdlib.Option.get
end

module List = struct
  include List
  let rec fold_left_es f acc = function
    | [] ->
       return acc
    | x :: xs ->
       f acc x >>=? fun acc -> (fold_left_es [@ocaml.tailcall]) f acc xs
end
