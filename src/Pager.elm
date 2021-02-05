module Pager exposing
  ( Pager
  , empty, fromList
  , currentPage
  , searchFor
  , hasPrev, hasNext
  , prev, next
  )


type Pager a =
  Pager (PagerInfo a)


type alias PagerInfo a =
  { data : List a
  , query : String
  , perPage : Int
  , page : Int
  , toSearchTerm : a -> String
  }


empty : Pager a
empty =
  fromList (always "") 1 []


fromList : (a -> String) -> Int -> List a -> Pager a
fromList toSearchTerm perPage data =
  Pager
    { data = data
    , query = ""
    , perPage = clamp 1 100 perPage
    , page = 1
    , toSearchTerm = toSearchTerm
    }


currentPage : Pager a -> List a
currentPage (Pager info) =
  let
    { data, page } =
      filter info
  in
  data
    |> List.drop ((page - 1) * info.perPage)
    |> List.take info.perPage


searchFor : String -> Pager a -> Pager a
searchFor query (Pager info) =
  let
    infoWithNewQuery =
      { info | query = query }

    { page } =
      filter infoWithNewQuery
  in
  Pager { infoWithNewQuery | page = page }


hasPrev : Pager a -> Bool
hasPrev (Pager info) =
  let
    { page } =
      filter info
  in
  page > 1


hasNext : Pager a -> Bool
hasNext (Pager info) =
  let
    { pages, page } =
      filter info
  in
  page < pages


prev : Pager a -> Pager a
prev ((Pager info) as pager) =
  let
    { page } =
      filter info
  in
  if page > 1 then
    Pager { info | page = page - 1 }
  else
    pager


next : Pager a -> Pager a
next ((Pager info) as pager) =
  let
    { pages, page } =
      filter info
  in
  if page < pages then
    Pager { info | page = page + 1 }
  else
    pager


-- HELPERS


filter : PagerInfo a -> { data: List a, pages: Int, page: Int }
filter info =
  let
    isSimilar elem =
      String.contains
        (String.toLower info.query)
        (String.toLower (info.toSearchTerm elem))

    data =
      List.filter isSimilar info.data

    total =
      List.length data

    pages =
      numberOfPages total info.perPage

    page =
      if total == 0 then
        1
      else
        clamp 1 pages info.page
  in
  { data = data
  , pages = pages
  , page = page
  }


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
