module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Navigation
import WebSocket


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> WebSocket.listen echoServer ServerResponse)
        }


echoServer : String
echoServer =
    "wss://echo.websocket.org"


type alias Model =
    { history : List Navigation.Location
    , username : String
    , password : String
    , status : String
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model [ location ] "" "" "Not logged in"
    , Cmd.none
    )


type Msg
    = UrlChange Navigation.Location
    | InputUsername String
    | InputPassword String
    | Submit
    | ServerResponse String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            { model | history = location :: model.history } ! [ Cmd.none ]

        InputUsername newUsername ->
            { model | username = newUsername } ! [ Cmd.none ]

        InputPassword newPassword ->
            { model | password = newPassword } ! [ Cmd.none ]

        Submit ->
            model ! [ WebSocket.send echoServer "Logged in successfully!" ]

        ServerResponse message ->
            { model | status = message } ! [ Cmd.none ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Login" ]
        , usernameInput model
        , passwordInput model
        , loginButton
        , div [] [ strong [] [ text model.status ] ]
        , h1 [] [ text "Pages" ]
        , ul [] (List.map viewLink [ "bears", "cats", "dogs", "elephants", "fish" ])
        , h1 [] [ text "History" ]
        , ul [] (List.map viewLocation model.history)
        ]


usernameInput : Model -> Html Msg
usernameInput model =
    input
        [ type_ "text"
        , value model.username
        , onInput InputUsername
        , placeholder "User name"
        ]
        []


passwordInput : Model -> Html Msg
passwordInput model =
    input
        [ type_ "password"
        , value model.password
        , onInput InputPassword
        , placeholder "Password"
        ]
        []


loginButton : Html Msg
loginButton =
    button
        [ onClick Submit ]
        [ text "Log in" ]


viewLink : String -> Html Msg
viewLink name =
    li [] [ a [ href ("#" ++ name) ] [ text name ] ]


viewLocation : Navigation.Location -> Html Msg
viewLocation location =
    li [] [ text (location.pathname ++ location.hash) ]
