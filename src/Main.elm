module Main exposing (main)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed


main : Program () Model msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


-- MODEL


type alias Model =
    { projects : List Project
    , isLoading : Bool
    , loadFailed : Bool
    , page : Int
    , pageSize : Int
    , searchQuery : String
    }


type alias Project =
    { previewImageUrl : String
    , name : String
    , primaryUrl : String
    , description : String
    , repositoryUrl : Maybe String
    }


init : flags -> (Model, Cmd msg)
init _ =
  ( { projects = initProjects
    , isLoading = False
    , loadFailed = False
    , page = 0
    , pageSize = 5
    , searchQuery = ""
    }
  , Cmd.none
  )


initProjects : List Project
initProjects =
  [ { previewImageUrl = "data/images/elmify.png"
    , name = "Elmify - stats about your listening tastes"
    , primaryUrl = "https://elmify.netlify.app/"
    , description = "Display your top tracks and top artists based on your listening habits on Spotify. Discover which songs suit your listening tastes the best."
    , repositoryUrl = Just "https://github.com/jindrazak/Elmify"
    }
  , { previewImageUrl = "data/images/BFG_SpeedMath.png"
    , name = "Brain Floss Games | Speed Math"
    , primaryUrl = "http://brainflossgames.com/"
    , description = "With these digit-by-digit instructions and unlimited practice questions, learning speed math has never been easier."
    , repositoryUrl = Nothing
    }
  , { previewImageUrl = "data/images/unli-rev-flight.jpg"
    , name = "UNLI Reverse Flight Search"
    , primaryUrl = "https://unli.xyz/flights/"
    , description = "Search flights in reverse. Made in 10 days from zero Elm experience to deployment with elm-sortable-table, kirchner/elm-selectize, and ohanhi/remotedata-http"
    , repositoryUrl = Nothing
    }
  , { previewImageUrl = "data/images/fourseasons.png"
    , name = "Across the Four Seasons"
    , primaryUrl = "https://fourseasons.aularon.com"
    , description = "An exploration into Vivaldi's Four Seasons concerti"
    , repositoryUrl = Just "https://github.com/aularon/four-seasons"
    }
  , { previewImageUrl = "data/images/swshdex.png"
    , name = "Sword/Shield Pokedex - A Pokemon Sword/Shield Helper App"
    , primaryUrl = "https://dex.3digit.dev"
    , description = "Multi-function helper app built for Sword/Shield -- Type Matchups, Party Planner, and more"
    , repositoryUrl = Just "https://github.com/3digitdev/swshdex"
    }
  , { previewImageUrl = "data/images/noconf.png"
    , name = "NoConf - conference agenda builder application"
    , primaryUrl = "https://noconf.io"
    , description = "A.I. based conference scheduler with organizer dashboard built fully with Elm"
    , repositoryUrl = Nothing
    }
  , { previewImageUrl = "data/images/fretmaster.jpg"
    , name = "FretMaster"
    , primaryUrl = "https://fretmaster.app/"
    , description = "Guitar fretboard learning tool / game"
    , repositoryUrl = Just "https://github.com/dkarter/fretmaster-elm"
    }
  , { previewImageUrl = "data/images/odyssey.png"
    , name = "Odyssey"
    , primaryUrl = "https://odyssey.neophilus.net/"
    , description = "Next generation gallery. Exceptional images deserve an exceptional presentation."
    , repositoryUrl = Just "https://github.com/Libbum/Odyssey"
    }
  , { previewImageUrl = "data/images/SlimeBuddyScreenshot_1000x800.png"
    , name = "Slime Buddy"
    , primaryUrl = "https://slime-buddy.netlify.com/"
    , description = "A little buddy for your desktop"
    , repositoryUrl = Just "https://github.com/wolfadex/slime-buddy"
    }
  , { previewImageUrl = "data/images/dodge.png"
    , name = "Dodge"
    , primaryUrl = "https://wolfadex.github.io/dodge/"
    , description = "A simple doding game"
    , repositoryUrl = Just "https://github.com/wolfadex/dodge/"
    }
  , { previewImageUrl = "data/images/verwonderwereld.jpg"
    , name = "Naturalis Verwonderwereld"
    , primaryUrl = "https://www.verwonderpaspoort.nl/verwonderwereld"
    , description = "Interactive world for Dutch science museum aimed at elementary school students. Made by Driebit Amsterdam"
    , repositoryUrl = Nothing
    }
  ]


-- UPDATE


update : msg -> Model -> (Model, Cmd msg)
update _ model =
  ( model
  , Cmd.none
  )


-- VIEW


view : Model -> Html msg
view model =
  let
    disablePrev =
      model.page == 0

    disableNext =
      model.page * model.pageSize + model.pageSize >= List.length model.projects
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
            , viewPageButton disablePrev "Newer"
            , viewPageButton disableNext "Older"
            ]
        ]
    ]


viewSidebar : Model -> Html msg
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
            , value model.searchQuery
            , autofocus True
            -- , onInput UpdateSearchQuery
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
  let
    filterCriteria project =
      String.contains (String.toLower model.searchQuery) (String.toLower project.name)
  in
    if model.isLoading then
      [ ( "", h2 [] [ text "Loading" ] ) ]
    else if model.loadFailed then
      [ ( "", h2 [] [ text "Unable to load projects" ] ) ]
    else
      model.projects
        |> List.filter filterCriteria
        |> List.drop (model.page * model.pageSize)
        |> List.take model.pageSize
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


viewPageButton : Bool -> String -> Html msg
viewPageButton isDisabled label =
  let
    textColor =
      if isDisabled then
        "#e5e5e5"
      else
        "#5cb5cd"
  in
    button
      [ disabled isDisabled
      , class "builtwithelm-Button"
      ]
      [ text label ]
