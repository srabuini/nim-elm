port module Nim exposing (..)

import Bitwise
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import String exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }



-- MODEL

type Player
  = Computer
  | Human
  | None


type alias Model =
  { board: List Int
  , currentRow: Maybe Int
  , currentMatches: Int
  , player: Player
  , text: String
  , nextPlayerButtonDisabled: Bool
  , undoButtonDisabled: Bool
  }


init : ( Model, Cmd Msg)
init =
  ( emptyModel, Cmd.none )

nextButtonTitle =
  "Computer Moves"


initialText =
  "Move & press " ++ nextButtonTitle ++ ", or Just press " ++ nextButtonTitle


emptyModel : Model
emptyModel =
  Model [1, 3, 5, 7] Nothing 0 None initialText False True



-- UPDATE


type Msg
  = ComputerMoves
  | RemoveMatchesFromCurrentRow Int
  | GetRandomMatchesForRowAt Int
  | RemoveMatch Int
  | Restart
  | Undo


removeMatches : Int -> Int -> Model -> Model
removeMatches row matches model =
  { model |
    board =
      model.board
        |> (List.indexedMap (\i x -> if i == row then x - matches else x))
  }


removeMatch : Int -> Model -> Model
removeMatch row model =
  model
    |> removeMatches row 1
    |> setCurrentRow (Just row)
    |> setComputerTurn


validMove : Model -> Int -> Bool
validMove model row=
  case model.currentRow of
    Nothing ->
      True

    Just currentRow ->
      if currentRow == row then True else False


moveIfValid : Int -> Model -> Model
moveIfValid row model =
  if validMove model row then
    model
      |> removeMatch row
      |> incrementMatches
      |> setUndoButton
      |> addText ("Nice move, you're playing awesome, press " ++
                  nextButtonTitle ++ " when you finish.")
  else
    model |> addText "Same row, fella 🙄"


sumXor : List Int -> Int
sumXor list =
  list |> List.foldr (\a b -> Bitwise.xor a b ) 0


noWinnerMove : List Int -> Bool
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


rowsWithMatches : List Int -> List Int
rowsWithMatches board =
  board |> filterIndex (\n -> n > 0) 0 []


matchesInRow : Int -> List Int -> Maybe Int
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
    result = Bitwise.xor matches sum
  in
    if result < matches then
      Just (matches - result)
    else
      Nothing


winnerRows : List Int -> List (Maybe Int)
winnerRows list =
  list
    |> List.map (winnerNumber (sumXor list))


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
    (row, matches) = firstWinnerMove (winnerRows model.board) 0
  in
    case (row, matches) of
      ( Just row, Just matches ) ->
        model
          |> removeMatches row matches

      _ ->
        model


setCurrentRow : Maybe Int -> Model -> Model
setCurrentRow value model =
  { model |
    currentRow = value
  }


setHumanTurn : Model -> Model
setHumanTurn model =
  { model |
    player = Human
  }

setComputerTurn : Model -> Model
setComputerTurn model =
  { model |
    player = Computer
  }


addSmile : Model -> Model
addSmile model =
  if noWinnerMove model.board then
    { model |
      text = model.text ++ "😎"}
  else
    model


afterComputerMoves : Model -> Model
afterComputerMoves model =
  model
    |> setCurrentRow Nothing
    |> setHumanTurn
    |> setUndoButtonDisabled True
    |> addText  "You move!"
    |> addSmile
    |> checkIfFinished


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


incrementMatches : Model -> Model
incrementMatches model =
  { model | currentMatches = model.currentMatches + 1 }


setUndoButton : Model -> Model
setUndoButton model =
    model |> setUndoButtonDisabled (model.currentMatches == 0)


setUndoButtonDisabled : Bool -> Model -> Model
setUndoButtonDisabled value model =
  { model | undoButtonDisabled = value }


undo : Model -> Model
undo model =
  case model.currentRow of
    Nothing ->
      model

    Just row ->
      model
        |> addMatch row
        |> addText "I think it was a good move..."
        |> setUndoButton
        |> setHumanTurn


addMatch : Int -> Model -> Model
addMatch row model =
  case model.currentMatches of
    0 ->
      model

    _ ->
      let
        newBoard = model.board |> List.indexedMap (\i x -> if i == row then x + 1 else x)
        matches = model.currentMatches - 1
        currentRow = if matches == 0 then Nothing else model.currentRow
      in
        { model
          | board = newBoard
          , currentMatches = matches
          , currentRow = currentRow}



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    RemoveMatch row ->
      ( model
        |> moveIfValid row
        |> checkIfFinished
        , Cmd.none
      )

    ComputerMoves ->
      case model.player of
        Human ->
          ( model
            |> addText "Hey, You move!", Cmd.none
          )

        _ ->
          if noWinnerMove model.board then
            let
              maxIndex = List.length (rowsWithMatches model.board) - 1
            in
              ( model, Random.generate GetRandomMatchesForRowAt (Random.int 0 maxIndex) )
          else
            ( model
              |> winnerMove
              |> afterComputerMoves, Cmd.none
            )

    GetRandomMatchesForRowAt index ->
      case rowsWithMatches model.board |> get index of
        Nothing ->
          ( model, Cmd.none )

        Just row ->
          case matchesInRow row model.board of
            Nothing ->
              ( model, Cmd.none )

            Just matches ->
              ( { model | currentRow = Just row }
              , Random.generate RemoveMatchesFromCurrentRow (Random.int 1 matches)
              )

    RemoveMatchesFromCurrentRow matches ->
      case model.currentRow of
        Just row ->
          ( model
            |> removeMatches row matches
            |> afterComputerMoves, Cmd.none
          )

        Nothing ->
          ( model, Cmd.none )

    Undo ->
      ( model
        |> undo, Cmd.none
      )

    Restart ->
      ( emptyModel, Cmd.none )



-- VIEW


rowContent row elements =
  div []
    (List.map (\l -> img [ onClick (RemoveMatch row), src "img/match.png" ] []) (List.range 0 (elements - 1)))


view : Model -> Html Msg
view model =
  div [ class "row" ] [
    div [ class "twelve column"] [
      ul []
        (List.indexedMap (\row elements -> li [] [ rowContent row elements ]) model.board)
      , div [ class "centered" ] [
          text model.text
        ]
      , div [ class "centered" ] [
          button [ onClick Restart ][ text "Restart" ]
        , button [ onClick Undo, disabled model.undoButtonDisabled ][ text "Undo" ]
        , button [ onClick ComputerMoves, disabled model.nextPlayerButtonDisabled ][ text nextButtonTitle ]
      ]
    ]
  ]
