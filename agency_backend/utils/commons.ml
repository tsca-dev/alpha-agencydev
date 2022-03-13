module Hexbytes = struct
  let prefixed_hex_of_bytes bytes =
    Format.asprintf "0x%a"
      Hex.pp (Hex.of_bytes bytes)

  let hex_of_bytes bytes =
    Format.asprintf "%a"
      Hex.pp (Hex.of_bytes bytes)

  let bytes_of_hex str =
    Hex.to_bytes (`Hex str)

  let bytes_of_prefixed_hex s =
    Hex.to_bytes (`Hex (String.sub s 2 (String.length s - 2)))
end include Hexbytes
