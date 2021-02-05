module Main exposing (main)


import Api
import Browser
import Browser.Dom as Dom
import Data exposing (Project)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as E
import Html.Keyed
import Http
import Pager exposing (Pager)
import Task


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


-- MODEL


type alias Model =
  { pager : Pager Project
  , pageSize : Int
  , isLoading : Bool
  , loadFailed : Bool
  , query : String
  }


init : flags -> (Model, Cmd Msg)
init _ =
  ( { pager = Pager.empty
    , pageSize = 5
    , isLoading = True
    , loadFailed = False
    , query = ""
    }
  , Api.fetchProjects GotProjects
  )


-- UPDATE


type Msg
  = PressedPrev
  | PressedNext
  | ScrolledToTop
  | EnteredQuery String
  | GotProjects (Result Http.Error (List Project))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PressedPrev ->
      ( { model | pager = Pager.prev model.pager }
      , scrollToTop
      )

    PressedNext ->
      ( { model | pager = Pager.next model.pager }
      , scrollToTop
      )

    ScrolledToTop ->
      ( model
      , Cmd.none
      )

    EnteredQuery query ->
      ( { model
        | pager = Pager.searchFor query model.pager
        , query = query
        }
      , scrollToTop
      )

    GotProjects (Ok projects) ->
      ( { model
        | pager = Pager.fromList .name model.pageSize projects
        , isLoading = False
        , loadFailed = False
        }
      , Cmd.none
      )

    GotProjects (Err _) ->
      ( { model
        | pager = Pager.empty
        , isLoading = False
        , loadFailed = True
        }
      , Cmd.none
      )


scrollToTop : Cmd Msg
scrollToTop =
  Task.perform (always ScrolledToTop) (Dom.setViewport 0 0)


-- VIEW


view : Model -> Html Msg
view model =
  let
    disablePrev =
      not (Pager.hasPrev model.pager)

    disableNext =
      not (Pager.hasNext model.pager)
  in
  div
    [ class "builtwithelm-Container" ]
    [ viewSidebar model
    , div
        [ class "builtwithelm-Content" ]
        [ Html.Keyed.node "div"
            [ class "builtwithelm-ListContainer" ]
          <|
            viewList model
        , div
            [ class "builtwithelm-Paging" ]
            [ viewPageSizeSelect model.pageSize [ 5, 25, 50, 100 ]
            , viewPageButton PressedPrev disablePrev "Newer"
            , viewPageButton PressedNext disableNext "Older"
            ]
        ]
    ]


viewSidebar : Model -> Html Msg
viewSidebar model =
  div [ class "builtwithelm-Sidebar" ]
    [ div [ class "builtwithelm-SidebarHeader" ]
        [ div
            [ class "builtwithelm-SidebarLogoContainer" ]
            [ a [ href "/" ]
                [ img [ src "assets/logo.svg", class "builtwithelm-Logo" ] [] ]
            ]
        , h1 []
            [ a
                [ href "/"
                , class "builtwithelm-BuiltWithLink"
                ]
                [ span
                    [ class "builtwithelm-BuiltWithText" ]
                    [ text "builtwith" ]
                , span [] [ text "elm" ]
                ]
            ]
        ]
    , div [ class "builtwithelm-SearchContainer" ]
        [ input
            [ type_ "text"
            , placeholder "Search"
            , value model.query
            , autofocus True
            , E.onInput EnteredQuery
            , class "builtwithelm-SearchInput"
            ]
            []
        ]
    , div [ class "builtwithelm-SubmitProject" ]
        [ h3
            [ class "builtwithelm-SubmitProjectHeader" ]
            [ text "Submit a project" ]
        , p []
            [ span [] [ text "Submit a pull request or post an issue to " ]
            , a [ href "https://github.com/elm-community/builtwithelm", target "_blank" ] [ text "the Github repo" ]
            , span [] [ text ". Please include a screenshot and ensure it is " ]
            , strong [] [ text "1000px x 800px" ]
            , span [] [ text "." ]
            ]
        ]
    , div
        [ class "builtwithelm-BuiltBy" ]
        [ span [] [ text "Built by " ]
        , a [ href "https://github.com/lukewestby", target "_blank" ] [ text "Luke Westby" ]
        , span [] [ text " and " ]
        , a [ href "https://github.com/elm-community/builtwithelm/graphs/contributors", target "_blank" ] [ text "the amazing Elm community." ]
        ]
    ]


viewList : Model -> List (String, Html msg)
viewList model =
  if model.isLoading then
    [ ( "", h2 [] [ text "Loading" ] ) ]
  else if model.loadFailed then
    [ ( "", h2 [] [ text "Unable to load projects" ] ) ]
  else
    model.pager
      |> Pager.currentPage
      |> List.map (\p -> ( p.primaryUrl, viewProject p ))


viewProject : Project -> Html msg
viewProject project =
  div
    [ class "builtwithelm-Project" ]
    [ div
        [ class "builtwithelm-ProjectHeader" ]
        [ a
            [ href project.primaryUrl
            , target "_blank"
            , class "builtwithelm-Link"
            ]
            [ h2 []
                [ text project.name ]
            ]
        , viewOpenSourceLink project
        ]
    , p [] [ text project.description ]
    , div
        [ class "builtwithelm-ProjectScreenshotShell" ]
        [ img
            [ src project.previewImageUrl
            , class "builtwithelm-ProjectImage"
            ]
            []
        ]
    ]


viewOpenSourceLink : Project -> Html msg
viewOpenSourceLink project =
  case project.repositoryUrl of
    Just url ->
      a
        [ href url
        , target "_blank"
        , class "builtwithelm-Link"
        ]
        [ img
            [ src "assets/github.svg"
            , class "builtwithelm-GithubLogo"
            ]
            []
        ]

    Nothing ->
      span [] []


viewPageSizeSelect : Int -> List Int -> Html msg
viewPageSizeSelect current options =
  let
    toOption i =
      option [ value <| String.fromInt i ] [ text <| String.fromInt i ]
  in
  div [ class "builtwithelm-Dropdown" ]
      [ label [] [ text "Page size" ]
      , select [ value <| String.fromInt current ]
          (List.map toOption options)
      ]


viewPageButton : Msg -> Bool -> String -> Html Msg
viewPageButton onPress isDisabled label =
  if isDisabled then
    button
      [ disabled True
      , class "builtwithelm-Button"
      ]
      [ text label ]
  else
    button
      [ E.onClick onPress
      , class "builtwithelm-Button"
      ]
      [ text label ]
