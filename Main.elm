port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Animation exposing (px)
import Color
import Time
import Mouse
import Debug
import Json.Decode


port saveFlags : Flags -> Cmd msg


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { stiffness : Float
    , damping : Float
    }


type alias Dest =
    { x : Int, y : Int }


type alias Model =
    { ball : Animation.State
    , dest : Dest
    , flags : Flags
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { ball =
            Animation.styleWith
                (Animation.spring
                    { stiffness = flags.stiffness
                    , damping = flags.damping
                    }
                )
                [ Animation.translate (px 0) (px 0)
                ]
      , dest = { x = 0, y = 0 }
      , flags = flags
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.ball ]


type Msg
    = ChangeStiffness String
    | MouseMove Mouse.Position
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
                , dest = { x = pos.x, y = pos.y }
              }
            , Cmd.none
            )

        ChangeStiffness valueStr ->
            let
                newStiffness =
                    Result.withDefault 20 (String.toFloat valueStr)

                flags =
                    model.flags

                newFlags =
                    { flags | stiffness = newStiffness }

                ball =
                    model.ball

                newBall =
                    Animation.styleWith
                        (Animation.spring newFlags)
                        [ Animation.translate (px (toFloat model.dest.x)) (px (toFloat model.dest.y))
                        ]
            in
                ( { model
                    | flags = newFlags
                    , ball = newBall
                  }
                , saveFlags newFlags
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
            ([ class "ball" ] ++ Animation.render model.ball)
            []
        , div
            [ class "config" ]
            [ table
                [ class "config-table" ]
                [ tr []
                    [ td [ class "config-table-term" ] [ text "Spring" ]
                    , td [ class "config-table-value" ] [ text (toString model.flags.stiffness) ]
                    , td
                        [ class "config-list-input" ]
                        [ input
                            [ type_ "range"
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "500"
                            , onInput ChangeStiffness
                            , defaultValue (toString model.flags.stiffness)
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


onMouseMove : Attribute Msg
onMouseMove =
    on "mousemove"
        (Json.Decode.map MouseMove Mouse.position)
