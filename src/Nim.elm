module Nim exposing (..)

import Bitwise
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type Player
    = Computer
    | Human
    | None


type alias Board =
    List Int


type ComputerMove
    = Random Int Int
    | Winner


type alias Model =
    { board : Board
    , currentRow : Maybe Int
    , currentMatches : Int
    , player : Player
    , text : String
    , nextPlayerButtonDisabled : Bool
    , undoButtonDisabled : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model [ 1, 3, 5, 7 ] Nothing 0 None initialText False True, Cmd.none )


nextButtonTitle : String
nextButtonTitle =
    "Computer Moves"


initialText : String
initialText =
    "Move & press " ++ nextButtonTitle ++ ", or Just press " ++ nextButtonTitle



-- UPDATE


type Msg
    = ComputerMoves
    | RemoveMatchesRandom Int
    | GenerateRandomMatchesForRowAt Int
    | RemoveMatch Int
    | Restart
    | Undo


removeMatchesRandom : Int -> Int -> Model -> Model
removeMatchesRandom row matches model =
    { model
        | board =
            model.board
                |> List.indexedMap
                    (\i x ->
                        if i == row then
                            x - matches
                        else
                            x
                    )
    }


removeMatch : Int -> Model -> Model
removeMatch row model =
    model
        |> removeMatchesRandom row 1
        |> setCurrentRow (Just row)
        |> setComputerTurn


validMove : Model -> Int -> Bool
validMove model row =
    case model.currentRow of
        Nothing ->
            True

        Just currentRow ->
            if currentRow == row then
                True
            else
                False


moveIfValid : Int -> Model -> Model
moveIfValid row model =
    if validMove model row then
        model
            |> removeMatch row
            |> incrementMatches
            |> setUndoButton
            |> addText
                ("Nice move, you're playing awesome, press "
                    ++ nextButtonTitle
                    ++ " when you finish."
                )
    else
        model |> addText "Same row, fella \x1F644"


sumXor : Board -> Int
sumXor board =
    board |> List.foldr (\a b -> Bitwise.xor a b) 0


noWinnerMove : Board -> Bool
noWinnerMove board =
    if sumXor board == 0 then
        True
    else
        False


findIndex : (Int -> Bool) -> List Int -> Maybe Int
findIndex predicate list =
    findIndex_ predicate list 0


findIndex_ : (Int -> Bool) -> List Int -> Int -> Maybe Int
findIndex_ predicate list index =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just index
            else
                findIndex_ predicate rest (index + 1)


filterIndex : (Int -> Bool) -> Int -> List Int -> List Int -> List Int
filterIndex predicate index newList list =
    case list of
        [] ->
            newList

        first :: rest ->
            if predicate first then
                filterIndex predicate (index + 1) (index :: newList) rest
            else
                filterIndex predicate (index + 1) newList rest


rowsWithMatches : Board -> List Int
rowsWithMatches board =
    board |> filterIndex (\n -> n > 0) 0 []


matchesInRow : Int -> Board -> Maybe Int
matchesInRow index board =
    get index board


get : Int -> List Int -> Maybe Int
get index list =
    list
        |> List.drop index
        |> List.head


winnerNumber : Int -> Int -> Maybe Int
winnerNumber sum matches =
    let
        result =
            Bitwise.xor matches sum
    in
    if result < matches then
        Just (matches - result)
    else
        Nothing


winnerRows : Board -> List (Maybe Int)
winnerRows board =
    board
        |> List.map (winnerNumber (sumXor board))


firstWinnerMove : List (Maybe Int) -> Int -> ( Maybe Int, Maybe Int )
firstWinnerMove list index =
    case list of
        [] ->
            ( Nothing, Nothing )

        first :: rest ->
            case first of
                Just first ->
                    ( Just index, Just first )

                Nothing ->
                    firstWinnerMove rest (index + 1)


winnerMove : Model -> Model
winnerMove model =
    let
        ( row, matches ) =
            firstWinnerMove (winnerRows model.board) 0
    in
    case ( row, matches ) of
        ( Just row, Just matches ) ->
            model
                |> removeMatchesRandom row matches

        _ ->
            model


computerPlay : ComputerMove -> Model -> Model
computerPlay kind model =
    model
        |> computerMove kind
        |> setCurrentRow Nothing
        |> resetCurrentMatches
        |> setHumanTurn
        |> setUndoButtonDisabled True
        |> addText "You move!"
        |> addSmile
        |> checkIfFinished


computerMove : ComputerMove -> Model -> Model
computerMove kind model =
    case kind of
        Winner ->
            model
                |> winnerMove

        Random row matches ->
            model
                |> removeMatchesRandom row matches


setCurrentRow : Maybe Int -> Model -> Model
setCurrentRow value model =
    { model
        | currentRow = value
    }


resetCurrentMatches : Model -> Model
resetCurrentMatches model =
    { model
        | currentMatches = 0
    }


setHumanTurn : Model -> Model
setHumanTurn model =
    { model
        | player = Human
    }


setComputerTurn : Model -> Model
setComputerTurn model =
    { model
        | player = Computer
    }


addSmile : Model -> Model
addSmile model =
    if noWinnerMove model.board then
        { model
            | text = model.text ++ "😎"
        }
    else
        model


addWinner : Model -> Model
addWinner model =
    case model.player of
        None ->
            model

        Computer ->
            model
                |> addText "You won..."

        Human ->
            model
                |> addText "I won!"


nextPlayerButtonDisabled : Model -> Model
nextPlayerButtonDisabled model =
    { model | nextPlayerButtonDisabled = True }


checkIfFinished : Model -> Model
checkIfFinished model =
    if model.board |> List.all (\n -> n == 0) then
        model
            |> addWinner
            |> nextPlayerButtonDisabled
            |> setUndoButtonDisabled True
    else
        model


addText : String -> Model -> Model
addText message model =
    { model | text = message }


returnText : String -> Model -> ( Model, Cmd Msg )
returnText message model =
    ( model
        |> addText message
    , Cmd.none
    )


incrementMatches : Model -> Model
incrementMatches model =
    { model | currentMatches = model.currentMatches + 1 }


setUndoButton : Model -> Model
setUndoButton model =
    if model.currentMatches == 0 then
        model
            |> setUndoButtonDisabled True
            |> setHumanTurn
    else
        model
            |> setUndoButtonDisabled False
            |> setComputerTurn


setUndoButtonDisabled : Bool -> Model -> Model
setUndoButtonDisabled value model =
    { model | undoButtonDisabled = value }


undo : Model -> ( Model, Cmd Msg )
undo model =
    case model.currentRow of
        Nothing ->
            ( model, Cmd.none )

        Just row ->
            ( model
                |> addMatch row
                |> addText "I think that was a good move..."
                |> setUndoButton
            , Cmd.none
            )


addMatch : Int -> Model -> Model
addMatch row model =
    case model.currentMatches of
        0 ->
            model

        _ ->
            let
                newBoard =
                    model.board
                        |> List.indexedMap
                            (\i x ->
                                if i == row then
                                    x + 1
                                else
                                    x
                            )

                matches =
                    model.currentMatches - 1

                currentRow =
                    if matches == 0 then
                        Nothing
                    else
                        model.currentRow
            in
            { model
                | board = newBoard
                , currentMatches = matches
                , currentRow = currentRow
            }


humanRemovesMatch : Int -> Model -> ( Model, Cmd Msg )
humanRemovesMatch row model =
    ( model
        |> moveIfValid row
        |> checkIfFinished
    , Cmd.none
    )


computerMoves : Model -> ( Model, Cmd Msg )
computerMoves model =
    case model.player of
        Human ->
            model
                |> returnText "Hey, You move!"

        _ ->
            if noWinnerMove model.board then
                let
                    maxIndex =
                        List.length (rowsWithMatches model.board) - 1
                in
                ( model, Random.generate GenerateRandomMatchesForRowAt (Random.int 0 maxIndex) )
            else
                ( model
                    |> computerPlay Winner
                , Cmd.none
                )


generateRandomMatches : Int -> Model -> ( Model, Cmd Msg )
generateRandomMatches index model =
    case rowsWithMatches model.board |> get index of
        Nothing ->
            ( model, Cmd.none )

        Just row ->
            case matchesInRow row model.board of
                Nothing ->
                    ( model, Cmd.none )

                Just matches ->
                    ( { model | currentRow = Just row }
                    , Random.generate RemoveMatchesRandom (Random.int 1 matches)
                    )


removeFromCurrentRow : Int -> Model -> ( Model, Cmd Msg )
removeFromCurrentRow matches model =
    case model.currentRow of
        Just row ->
            ( model
                |> computerPlay (Random row matches)
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RemoveMatch row ->
            model
                |> humanRemovesMatch row

        ComputerMoves ->
            model
                |> computerMoves

        GenerateRandomMatchesForRowAt index ->
            model
                |> generateRandomMatches index

        RemoveMatchesRandom matches ->
            model
                |> removeFromCurrentRow matches

        Undo ->
            model
                |> undo

        Restart ->
            init



-- VIEW


rowContent : Int -> Int -> Html Msg
rowContent row elements =
    div []
        (List.map (\l -> img [ onClick (RemoveMatch row), src "img/match.png" ] []) (List.range 0 (elements - 1)))


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "twelve column" ]
            [ ul []
                (List.indexedMap (\row elements -> li [] [ rowContent row elements ]) model.board)
            , div [ class "centered" ]
                [ text model.text
                ]
            , div [ class "centered" ]
                [ button [ onClick Restart ] [ text "Restart" ]
                , button [ onClick Undo, disabled model.undoButtonDisabled ] [ text "Undo" ]
                , button [ onClick ComputerMoves, disabled model.nextPlayerButtonDisabled ] [ text nextButtonTitle ]
                ]
            ]
        ]
