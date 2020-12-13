-- | Модуль для типов в игре
module Types (PlayerMark, Cell, Board, GameState) where

-- | Отметка о том, каким игроком занята клетка
data PlayerMark = First | Second

-- | Клетка на игровой доске
data Cell = Owned PlayerMark | Free

-- | Игровая доска
data Board = Board [[Cell]]

-- | Состояние игры
data GameState = GameState
  { board :: Board }