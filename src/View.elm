module View exposing (..)

import String
import Html exposing (Html, button, div, text, h2, a, img, span, strong, h1, p, h3, input, label, select, option)
import Html.Attributes exposing (class, style, disabled, href, src, target, type_, placeholder, value, autofocus)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Model exposing (Model, Project)
import Update exposing (Msg(..))


view : Model -> Html Msg
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
                    [ viewPageSizeSelect SetPageSize model.pageSize [ 5, 25, 50, 100 ]
                    , viewPageButton Prev disablePrev "Newer"
                    , viewPageButton Next disableNext "Older"
                    ]
                ]
            ]


viewPageButton : Msg -> Bool -> String -> Html Msg
viewPageButton msg isDisabled label =
    let
        textColor =
            if isDisabled then
                "#e5e5e5"
            else
                "#5cb5cd"
    in
        button
            [ onClick msg
            , disabled isDisabled
            , class "builtwithelm-Button"
            ]
            [ text label ]


viewPageSizeSelect : (String -> Msg) -> Int -> List Int -> Html Msg
viewPageSizeSelect msg current options =
    let
        toOption i =
            option [ value <| toString i ]
                [ text <| toString i ]
    in
        div [ class "builtwithelm-Dropdown" ]
            [ label []
                [ text "Page size" ]
            , select [ value <| toString current, onInput msg ]
                (List.map toOption options)
            ]


viewList : Model -> List ( String, Html Msg )
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


viewOpenSourceLink : Project -> Html Msg
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


viewProject : Project -> Html Msg
viewProject project =
    div
        [ class "builtwithelm-Styles.Project" ]
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
                , value model.searchQuery
                , autofocus True
                , onInput UpdateSearchQuery
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
