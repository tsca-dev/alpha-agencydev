open Book_intf
open BookTypes
open TmplversionTypes
open Common

module Frozen = Book_of_frozen.Frozen
module Graveyard = Book_of_graveyard.Graveyard
module Crowdfunding = Book_of_crowdfunding.Crowdfunding

let available_books
    : (string (* bookhash *)
       * book (* newer to older wrt tmplhash *)) list = [
    Frozen.book_entry;
    Crowdfunding.book_entry;
    Graveyard.book_entry;
  ] |&> (fun b -> b.entry.bookhash, b)

let advertized_books : book_entry list = [
    Frozen.book_entry.entry;
    Crowdfunding.book_entry.entry;
    Graveyard.book_entry.entry;
  ]
let all_providers : (string (* providerident *) * provider) list = [
    tsca_team;
  ] |&> (fun p -> p.providerident, p)
let available_tmplversions :
      (string (* tmplhash *) *
       tmplversion (* newer to older *) list) list = [
    Frozen.tmplhash, [ Frozen.tmplversion_001 ];
    Graveyard.tmplhash, [ Graveyard.tmplversion_001 ];
    Crowdfunding.tmplhash, [ Crowdfunding.tmplversion_001 ];
  ]

let books_for_template tmplhash =
  available_books
  |&> (fun (_, ({ entry; _ } as bk)) -> entry.tmplhash, bk)
  |> List.filter_map (fun (h, e) -> if h=tmplhash then Some e else None)

