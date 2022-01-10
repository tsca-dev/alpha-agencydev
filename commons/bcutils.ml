open Tezos_shell_services

let chain_of_string str = Chain_services.parse_chain str |> Result.get_ok

let block_of_string str = Block_services.parse_block str |> Result.get_ok
