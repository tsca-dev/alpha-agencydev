open Book_intf

type book = BookTypes.t

let tez x =
  Q.(of_float x |> mul (of_int 1_000_000))
  |> Q.to_int64

let tsca_team : BookTypes.provider = {
      providerident = "tsca-team";
      display_name = "TSCA Team";
      introduction = "The TSCA Prototyping Team";
      website = "https://github.com/tsca-dev";
      contact = "https://github.com/tsca-dev/meta/issues/new";
    }
