module Main exposing (main)


import Api
import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as E
import Html.Keyed
import Http
import Pager exposing (Pager)
import Project exposing (Project)
import RemoteData exposing (RemoteData(..))
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
  { remoteData : RemoteData (Pager Project)
  , pageSize : Int
  , query : String
  }


init : flags -> (Model, Cmd Msg)
init _ =
  ( { remoteData = Loading
    , pageSize = 5
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
  | ChangedPageSize String
  | GotProjects (Result Http.Error (List Project))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PressedPrev ->
      ( { model | remoteData = RemoteData.map Pager.prev model.remoteData }
      , scrollToTop
      )

    PressedNext ->
      ( { model | remoteData = RemoteData.map Pager.next model.remoteData }
      , scrollToTop
      )

    ScrolledToTop ->
      ( model
      , Cmd.none
      )

    EnteredQuery query ->
      ( { model
        | remoteData = RemoteData.map (Pager.searchFor query) model.remoteData
        , query = query
        }
      , scrollToTop
      )

    ChangedPageSize pageSizeString ->
      ( let
          pageSize =
            String.toInt pageSizeString
              |> Maybe.withDefault 5

          remoteData =
            RemoteData.map (Pager.withPerPage pageSize) model.remoteData
        in
        { model | remoteData = remoteData, pageSize = pageSize }
      , Cmd.none
      )

    GotProjects (Ok projects) ->
      ( { model
        | remoteData = Success (Pager.fromList model.pageSize .name projects)
        }
      , Cmd.none
      )

    GotProjects (Err _) ->
      ( { model | remoteData = Failure }
      , Cmd.none
      )


scrollToTop : Cmd Msg
scrollToTop =
  Task.perform (always ScrolledToTop) (Dom.setViewport 0 0)


-- VIEW


view : Model -> Html Msg
view model =
  case model.remoteData of
    Loading ->
      div
        [ class "builtwithelm-Container" ]
        [ viewSidebar model.query
        , div
            [ class "builtwithelm-Content" ]
            [ div [ class "builtwithelm-ListContainer" ]
                [ text "Loading..." ]
            ]
        ]

    Failure ->
      div
        [ class "builtwithelm-Container" ]
        [ viewSidebar model.query
        , div
            [ class "builtwithelm-Content" ]
            [ div [ class "builtwithelm-ListContainer" ]
                [ text "Unable to load projects" ]
            ]
        ]

    Success pager ->
      let
        page =
          Pager.currentPage pager

        disablePrev =
          not page.hasPrev

        disableNext =
          not page.hasNext
      in
      div
        [ class "builtwithelm-Container" ]
        [ viewSidebar model.query
        , div
            [ class "builtwithelm-Content" ]
            [ Html.Keyed.node
                "div"
                [ class "builtwithelm-ListContainer" ]
                (viewProjects page.data)
            , div
                [ class "builtwithelm-Paging" ]
                [ viewPageSizeSelect model.pageSize [ 5, 25, 50, 100 ]
                , viewPageButton PressedPrev disablePrev "Newer"
                , viewPageButton PressedNext disableNext "Older"
                ]
            ]
        ]


viewSidebar : String -> Html Msg
viewSidebar query =
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
            , value query
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


viewProjects : List Project -> List (String, Html msg)
viewProjects projects =
  List.map (\p -> (p.primaryUrl, viewProject p)) projects


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


viewPageSizeSelect : Int -> List Int -> Html Msg
viewPageSizeSelect current options =
  let
    toOption i =
      option [ value <| String.fromInt i ] [ text <| String.fromInt i ]
  in
  div [ class "builtwithelm-Dropdown" ]
      [ label [] [ text "Page size" ]
      , select
          [ value <| String.fromInt current
          , E.onInput ChangedPageSize
          ]
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
