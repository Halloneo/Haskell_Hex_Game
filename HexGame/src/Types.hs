-- | Модуль для типов в игре
module Types (
  PlayerMark,
  Cell,
  GameState,
  createPlayerMark,
  createEmptyCell,
  updateBoard,
  createEmptyBoard,
  initialGameState,
  makeMove,
  getNextPlayer,
  getCurrentBoard,
  updateState) where

-- | Отметка о том, каким игроком занята клетка
data PlayerMark = First | Second deriving (Eq, Show)

createPlayerMark :: Int -> PlayerMark
createPlayerMark num
    | num == 1 = First
    | num == 2 = Second

-- | Клетка на игровой доске
data Cell = Owned PlayerMark | Free deriving (Eq, Show)

createEmptyCell :: Cell
createEmptyCell = Free

createCellWithMark :: PlayerMark -> Cell
createCellWithMark mark = Owned mark

-- | Состояние игры
data GameState = GameState
  { board :: [[Cell]], currentPlayer :: PlayerMark }

createEmptyBoard :: [[Cell]]
createEmptyBoard = do
  let cell = createEmptyCell
  let row = [cell, cell, cell, cell, cell, cell, cell, cell, cell, cell, cell]
  [row, row, row, row, row, row, row, row, row, row, row]

updateBoard :: [[Cell]] -> (Int, Int) -> PlayerMark -> [[Cell]]
updateBoard board (row, column) value = do
  let beginning = take row board
  let rowWithEdit = take column (board!!row) ++ [createCellWithMark value] ++ drop (column + 1) (board!!row)
  let ending = drop (row + 1) board
  beginning ++ [rowWithEdit] ++ ending

initialGameState :: GameState
initialGameState = do
  let board = createEmptyBoard
  let mark = createPlayerMark 1
  GameState { board = board, currentPlayer = mark }

getNextPlayer :: PlayerMark -> PlayerMark
getNextPlayer mark
  | mark == First = Second
  | mark == Second = First

updateState :: GameState -> (Int, Int) -> GameState
updateState state coords = do
  let newBoard = updateBoard (board $ state) coords (currentPlayer $ state)
  let nextPlayer = getNextPlayer (currentPlayer $ state)
  GameState { board = newBoard, currentPlayer = nextPlayer }

makeMove :: GameState -> (Int, Int) -> GameState
makeMove state coords = do
  let x = fst coords
  let y = snd coords
  let currBoard = (board $ state)
  if (currBoard !! x !! y == Free) then
    updateState state coords
  else
    state

getCurrentBoard :: GameState -> [[Cell]]
getCurrentBoard state = do
  (board $ state)