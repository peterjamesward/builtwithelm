module Route exposing (Route(..), HomeParams, fromUrl)


import Url
import Url.Parser exposing (Parser, map, oneOf, parse, query)
import Url.Parser.Query as Query


type Route
  = Home HomeParams


type alias HomeParams =
  { query : Maybe String
  , pageNumber : Maybe Int
  }


fromUrl : Url.Url -> Maybe Route
fromUrl url =
  parse parser url


parser : Parser (Route -> a) a
parser =
  oneOf
    [ map Home (query homeParams)
    ]


homeParams : Query.Parser HomeParams
homeParams =
  Query.map2
    HomeParams
    (Query.string "q")
    (Query.int "page")
