module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Animation exposing (px)
import Color
import Time
import Mouse
import Json.Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { ball : Animation.State }


init : ( Model, Cmd Msg )
init =
    ( { ball =
            Animation.styleWith
                (Animation.spring
                    { stiffness = 200
                    , damping = 15
                    }
                )
                [ Animation.translate (px 0) (px 0)
                ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.ball ]


type Msg
    = MouseMove Mouse.Position
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        MouseMove pos ->
            ( { model
                | ball =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.translate (px (toFloat pos.x)) (px (toFloat pos.y))
                            ]
                        ]
                        model.ball
              }
            , Cmd.none
            )

        Animate animMsg ->
            ( { model | ball = Animation.update animMsg model.ball }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ onMouseMove
        , style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "background", "#eff" )
            ]
        ]
        [ div
            (Animation.render model.ball
                ++ [ style
                        [ ( "width", "200px" )
                        , ( "height", "200px" )
                        , ( "border-radius", "100%" )
                        , ( "cursor", "pointer" )
                        , ( "text-align", "center" )
                        , ( "background", "blue" )
                        , ( "position", "absolute" )
                        , ( "left", "-100px" )
                        , ( "top", "-100px" )
                        ]
                   ]
            )
            []
        ]


onMouseMove : Attribute Msg
onMouseMove =
    on "mousemove" (Json.Decode.map MouseMove Mouse.position)
