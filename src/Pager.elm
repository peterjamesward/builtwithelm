module Pager exposing
  ( Pager
  , empty, fromList
  , toList
  , hasPrev, hasNext
  , prev, next
  )


type Pager a =
  Pager
    { data : List a
    , perPage : Int
    , page : Int
    , total : Int
    }


empty : Pager a
empty =
  fromList 1 []


fromList : Int -> List a -> Pager a
fromList perPage data =
  Pager
    { data = data
    , perPage = clamp 1 100 perPage
    , page = 1
    , total = List.length data
    }


toList : Pager a -> List a
toList (Pager { data, perPage, page }) =
  data
    |> List.drop ((page - 1) * perPage)
    |> List.take perPage


hasPrev : Pager a -> Bool
hasPrev (Pager { page }) =
  page > 1


hasNext : Pager a -> Bool
hasNext (Pager { perPage, page, total }) =
  page < numberOfPages total perPage


prev : Pager a -> Pager a
prev ((Pager info) as pager) =
  if hasPrev pager then
    Pager { info | page = info.page - 1 }
  else
    pager


next : Pager a -> Pager a
next ((Pager info) as pager) =
  if hasNext pager then
    Pager { info | page = info.page + 1 }
  else
    pager


numberOfPages : Int -> Int -> Int
numberOfPages total perPage =
  let
    n =
      total // perPage
  in
  if modBy perPage total == 0 then
    n
  else
    n + 1
