import Lean.Elab
import Lean.Elab.Term
import Lean.Meta
import Lean.Parser

namespace TicTacToe

/-- Represents a player in Tic Tac Toe. -/
inductive Player
  | X | O
deriving Repr, BEq, Inhabited

instance : ToString Player where
  toString
    | .X => "X"
    | .O => "O"

/-- Represents a single square on the Tic Tac Toe board. -/
inductive Square
  | empty
  | occupied (player : Player)
deriving Repr, BEq, Inhabited

instance : ToString Square where
  toString
    | .empty => " "
    | .occupied p => toString p

@[simp]
def boardSize : Nat := 9

/-- Helper to get the index for a 2D coordinate. -/
@[simp]
def toIdx (row col : Nat) : Option Nat :=
  if row < 3 && col < 3 then some (row * 3 + col) else none

/-- Helper to get 2D coordinates from an index. -/
@[simp]
def fromIdx (idx : Nat) : Option (Nat × Nat) :=
  if idx < boardSize then some (idx / 3, idx % 3) else none

/-- Represents the 3x3 Tic Tac Toe board.
    We use a flat array of 9 squares, indexed 0-8. -/
abbrev Board := Array Square



/-- Ensures a board has the correct size. Primarily for internal consistency. -/
def mkBoard (squares : Array Square) : Option Board :=
  if squares.size == boardSize then some squares else none

def initialBoard : Board :=
  Array.replicate boardSize Square.empty

@[simp]
def getSquare (b : Board) (row col : Nat) : Option Square := do
  let idx ← toIdx row col
  b[idx]?

/-- Sets a square on the board. Returns `none` if the move is invalid (out of bounds or square already occupied). -/
def setSquare (b : Board) (row col : Nat) (s : Square) : Option Board :=
  match toIdx row col with
  | none => none
  | some idx =>
    match b[idx]? with
    | none => none -- Should not happen for a board of correct size
    | some currentSq =>
      if currentSq == Square.empty || s == Square.empty then -- Allow placing on empty or clearing a square
        some (b.setIfInBounds idx s) -- `setD` is safe as `idx` is within `0..boardSize-1`
      else
        none -- Square is already occupied and not being cleared

/-- Pretty print the board. -/
def ppBoard (b : Board) : String :=
  let getSqStr (r c : Nat) : String := toString (b.getD (toIdx r c).get! Square.empty)
  let rowStr (r : Nat) : String :=
    "│ " ++ (getSqStr r 0) ++
    " │ " ++ (getSqStr r 1) ++
    " │ " ++ (getSqStr r 2) ++ " │"
  "┌───┬───┬───┐\n" ++
  (rowStr 0) ++ "\n" ++
  "├───┼───┼───┤\n" ++
  (rowStr 1) ++ "\n" ++
  "├───┼───┼───┤\n" ++
  (rowStr 2) ++ "\n" ++
  "└───┴───┴───┘"

-- Custom Repr instance for prettier #eval output, defined after ppBoard
instance : Repr Board where
  reprPrec b _ := ppBoard b

instance : ToString Board where
  toString b := ppBoard b

#eval ppBoard initialBoard -- Still useful for testing ppBoard directly

/-- Attempts to make a move for the given player at (row, col).
    Returns the new board if the move is valid, otherwise `none`. -/
def Board.move (b : Board) (p : Player) (row col : Nat) : Option Board :=
  setSquare b row col (Square.occupied p)

/-- Represents the possible game outcomes. -/
inductive GameStatus
  /-- A player has won. -/
  | Win (player : Player)
  /-- The game is a draw. -/
  | Draw
  /-- The game is ongoing. -/
  | Ongoing
deriving Repr, BEq

/-- Pretty print the game status. -/
instance : ToString GameStatus where
  toString
    | .Win p => s!"Win {p}"
    | .Draw => "Draw"
    | .Ongoing => "Ongoing"

/-- Checks if a player has won on the current board. -/
def checkPlayerWin (b : Board) (p : Player) : Bool :=
  let s := Square.occupied p
  -- Check rows
  ( [0,1,2].all fun c => getSquare b 0 c == some s) ||
  ( [0,1,2].all fun c => getSquare b 1 c == some s) ||
  ( [0,1,2].all fun c => getSquare b 2 c == some s) ||
  -- Check columns
  ( [0,1,2].all fun r => getSquare b r 0 == some s) ||
  ( [0,1,2].all fun r => getSquare b r 1 == some s) ||
  ( [0,1,2].all fun r => getSquare b r 2 == some s) ||
  -- Check diagonals
  ( (getSquare b 0 0 == some s) && (getSquare b 1 1 == some s) && (getSquare b 2 2 == some s) ) ||
  ( (getSquare b 0 2 == some s) && (getSquare b 1 1 == some s) && (getSquare b 2 0 == some s) )

/-- Checks if the board is full (all squares are occupied). -/
def isBoardFull (b : Board) : Bool :=
  b.all fun sq => sq != Square.empty

/-- Determines the current status of the game. -/
def getGameStatus (b : Board) : GameStatus :=
  if checkPlayerWin b Player.X then GameStatus.Win Player.X
  else if checkPlayerWin b Player.O then GameStatus.Win Player.O
  else if isBoardFull b then GameStatus.Draw
  else GameStatus.Ongoing

/-! ## Custom Board Syntax
    The body of an `elab ... : term => do ...` rule must produce `TermElabM Expr`.
-/

/-- Cell. -/
declare_syntax_cat tttCell
/-- X cell. -/
syntax "X" : tttCell
/-- O cell. -/
syntax "O" : tttCell
/-- Empty cell. -/
syntax "_" : tttCell

/-- Row. -/
declare_syntax_cat tttRow
/-- A valid row is 3 cells separated by `|`. -/
syntax (name := tttRowRule) tttCell "|" tttCell "|" tttCell : tttRow

/-- Board. -/
declare_syntax_cat tttBoardSyntax
/-- A valid board is 3 rows. -/
syntax tttRow tttRow tttRow : tttBoardSyntax

/-- Elaborator for a single cell. -/
def elabTttCell (stx : Lean.Syntax) : Lean.Elab.Term.TermElabM Square :=
  match stx with
  | `(tttCell| X) => pure (.occupied .X)
  | `(tttCell| O) => pure (.occupied .O)
  | `(tttCell| _) => pure .empty
  | _ => Lean.throwError s!"unsupported cell syntax {stx}"

elab "board! " b:tttBoardSyntax : term => do
  let mut squaresArr : Array Square := #[]
  let rowNodes := #[b.raw.getArg 0, b.raw.getArg 1, b.raw.getArg 2]
  if b.raw.getNumArgs != 3 then
    Lean.throwError s!"Expected 3 row arguments for tttBoardSyntax, got {b.raw.getNumArgs}"

  for rowStxUnchecked in rowNodes do
    let rowStx := rowStxUnchecked
    if rowStx.isOfKind `TicTacToe.tttRowRule then
      if rowStx.getNumArgs == 5 then
        let cellStxsRaw := #[rowStx.getArg 0, rowStx.getArg 2, rowStx.getArg 4]
        if cellStxsRaw.size != 3 then -- Should always be 3
          Lean.throwError s!"Internal error: selected wrong number of cells from tttRow {rowStx}"
        for cellStx in cellStxsRaw do
          let square_val ← elabTttCell cellStx
          squaresArr := squaresArr.push square_val
      else
        Lean.throwError s!"Expected 5 arguments for tttRow (cell|cell|cell), got {rowStx.getNumArgs} in {rowStx}"
    else
      Lean.throwError s!"Expected a tttRow of kind 'TicTacToe.tttRowRule', got {rowStx.getKind}"

  if squaresArr.size != 9 then
    Lean.throwError s!"Failed to parse 9 squares for the board, got {squaresArr.size}"

  -- Construct the Syntax for the array literal, e.g., `#[...]`
  let squareTerms ← squaresArr.mapM fun sq =>
    match sq with
    | Square.empty => `(Square.empty)
    | Square.occupied Player.X => `(Square.occupied Player.X)
    | Square.occupied Player.O => `(Square.occupied Player.O)
  let arrSyntax ← `(#[$squareTerms,*])

  -- Elaborate this arrSyntax into an Expr, with an expected type.
  let expectedTypeSyntax ← `(TicTacToe.Board)
  let expectedTypeExpr ← Lean.Elab.Term.elabType expectedTypeSyntax
  let boardExpr ← Lean.Elab.Term.elabTerm arrSyntax (some expectedTypeExpr)

  return boardExpr

-- Example usage of the custom syntax:
#eval board!
  X | O | _
  _ | X | _
  O | _ | X


#eval (do
  let customBoard := board!
    X | _ | O
    X | O | _
    _ | _ | X
  IO.println "Custom board created by syntax:"
  IO.println (ppBoard customBoard)
  IO.println (repr (getGameStatus customBoard))
  return () : IO Unit)

-- Example game simulation (from before):
#eval (do
  IO.println "\n--- Game Simulation ---"
  let b0 := initialBoard
  IO.println "Initial board:"
  IO.println (ppBoard b0)

  match Board.move b0 Player.X 0 0 with
  | some b1 =>
    IO.println "Player X moves (0,0):"
    IO.println (ppBoard b1)
    match Board.move b1 Player.O 1 1 with
    | some b2 =>
      IO.println "Player O moves (1,1):"
      IO.println (ppBoard b2)
      match Board.move b2 Player.X 0 1 with
      | some b3 =>
        IO.println "Player X moves (0,1):"
        IO.println (ppBoard b3)
        match Board.move b3 Player.O 2 2 with
        | some b4 =>
          IO.println "Player O moves (2,2):"
          IO.println (ppBoard b4)
          match Board.move b4 Player.X 0 2 with -- X wins
          | some b5 =>
            IO.println "Player X moves (0,2) - X wins:"
            IO.println (ppBoard b5)
            IO.println ("Status: " ++ repr (getGameStatus b5))
          | none => IO.println "Invalid move for b5"
        | none => IO.println "Invalid move for b4"
      | none => IO.println "Invalid move for b3"
    | none => IO.println "Invalid move for b2"
  | none => IO.println "Invalid move for b1"
  return () : IO Unit)

-- Test draw (from before)
#eval (do
  IO.println "\n--- Draw Game Test ---"
  let mut board := initialBoard
  board := board.move .X 0 0 |>.getD board
  board := board.move .O 0 1 |>.getD board
  board := board.move .X 0 2 |>.getD board
  board := board.move .O 1 1 |>.getD board
  board := board.move .X 1 0 |>.getD board
  board := board.move .O 1 2 |>.getD board
  board := board.move .X 2 1 |>.getD board
  board := board.move .O 2 0 |>.getD board
  board := board.move .X 2 2 |>.getD board
  IO.println "Final board for draw test:"
  IO.println (ppBoard board)
  IO.println (s!"Status: {getGameStatus board}")
  return () : IO Unit)

-- Test win
#eval (do
  IO.println "\n--- Win Game Test ---"
  let mut board := initialBoard
  board := board.move .X 0 0 |>.getD board
  board := board.move .O 1 1 |>.getD board
  board := board.move .X 0 2 |>.getD board
  board := board.move .O 1 1 |>.getD board
  board := board.move .X 1 0 |>.getD board
  board := board.move .O 1 2 |>.getD board
  board := board.move .X 2 1 |>.getD board
  board := board.move .O 2 0 |>.getD board
  board := board.move .X 2 2 |>.getD board
  IO.println "Final board for win test:"
  IO.println (ppBoard board)
  IO.println (s!"Status: {getGameStatus board}")
  return () : IO Unit)

end TicTacToe
