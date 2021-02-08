module Route exposing (Route(..), Query, fromUrl)


import Url
import Url.Parser as P
import Url.Parser.Query as Q


type Route
  = Home Query


type alias Query =
  { search : Maybe String
  , page : Maybe Int
  }


fromUrl : Url.Url -> Maybe Route
fromUrl url =
  P.parse parser url


parser : P.Parser (Route -> a) a
parser =
  P.oneOf
    [ P.map Home (P.query query)
    ]


query : Q.Parser Query
query =
  Q.map2
    Query
    (Q.string "q")
    (Q.int "page")
