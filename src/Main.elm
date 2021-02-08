module Main exposing (main)


import Browser
import Browser.Navigation as Nav
import Html
import Route
import Screen.Home
import Url exposing (Url)


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    , onUrlRequest = ClickedLink
    , onUrlChange = ChangedUrl
    }


-- MODEL


type alias Model =
  { url : Url
  , key : Nav.Key
  , screen : Screen
  }


type Screen
  = Home Screen.Home.Model
  | NotFound


init : flags -> Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  fromUrl url
    |> Tuple.mapFirst (Model url key)


fromUrl : Url -> (Screen, Cmd Msg)
fromUrl url =
  case Route.fromUrl url of
    Just (Route.Home query) ->
      Screen.Home.init query
        |> Tuple.mapBoth Home (Cmd.map HomeMsg)

    Nothing ->
      ( NotFound
      , Cmd.none
      )


-- UPDATE


type Msg
  = ClickedLink Browser.UrlRequest
  | ChangedUrl Url
  | HomeMsg Screen.Home.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClickedLink urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model
          , Nav.pushUrl model.key (Url.toString url)
          )

        Browser.External href ->
          ( model
          , Nav.load href
          )

    ChangedUrl url ->
      ( { model | url = url }
      , Cmd.none
      )

    HomeMsg homeMsg ->
      case model.screen of
        Home homeModel ->
          let
            (newHomeModel, newHomeCmd) =
              Screen.Home.update homeMsg homeModel
          in
          ( { model | screen = Home newHomeModel }
          , Cmd.map HomeMsg newHomeCmd
          )

        _ ->
          (model, Cmd.none)


-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Built with Elm"
  , body =
      case model.screen of
        Home homeModel ->
          Screen.Home.view homeModel
            |> List.map (Html.map HomeMsg)

        NotFound ->
          [ Html.text "Not found" ]
  }
