module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Animation exposing (px)
import Color
import Time


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
                -- default easing is 0.2 seconds
                (Animation.easing
                    { duration = 0.2 * Time.second
                    , ease = (\x -> x ^ 2)
                    }
                )
                [ Animation.backgroundColor (Color.blue)
                ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.ball ]


type Msg
    = MouseEnter
    | MouseLeave
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        MouseEnter ->
            -- use default easing of 0.2 seconds
            ( { model
                | ball =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.backgroundColor (Color.orange)
                            ]
                        ]
                        model.ball
              }
            , Cmd.none
            )

        MouseLeave ->
            -- use new easing of 1 second
            ( { model
                | ball =
                    --Animation.interrupt
                    --    [ Animation.toWith
                    --        (Animation.easing
                    --            { duration = 1 * Time.second
                    --            , ease = (\x -> x)
                    --            }
                    --        )
                    --        [ Animation.backgroundColor (Color.blue)
                    --        ]
                    --    ]
                    --    model.ball
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.backgroundColor (Color.blue)
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
        (Animation.render model.ball
            ++ [ onMouseEnter MouseEnter
               , onMouseLeave MouseLeave
               , style
                    [ ( "width", "200px" )
                    , ( "height", "200px" )
                    , ( "border-radius", "100%" )
                    , ( "margin", "200px auto" )
                    , ( "cursor", "pointer" )
                    , ( "text-align", "center" )
                    ]
               ]
        )
        []
