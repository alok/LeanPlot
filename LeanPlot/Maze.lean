import Lean


-- Coordinates in a two dimensional grid. ⟨0,0⟩ is the upper left.
/-- Coordinates in a two dimensional grid. ⟨0,0⟩ is the upper left. -/
structure Coords where
  /-- column number -/
  x : Nat
  /-- row number -/
  y : Nat
deriving BEq

/-- Represents the state of a maze game, including the grid size, player position, and wall positions. -/
structure GameState where
  /-- coordinates of bottom-right cell -/
  size     : Coords
  /-- row and column of the player -/
  position : Coords
  /-- maze cells that are not traversible -/
  walls    : List Coords

/-- A game cell is a single character. -/
declare_syntax_cat game_cell
/-- A sequence of game cells. -/
declare_syntax_cat game_cell_sequence
/-- A row of game cells. -/
declare_syntax_cat game_row
/-- A horizontal border is a sequence of horizontal bars. -/
declare_syntax_cat horizontal_border
/-- A top row of a maze. -/
declare_syntax_cat game_top_row
/-- A bottom row of a maze. -/
declare_syntax_cat game_bottom_row

/-- The horizontal border is a sequence of horizontal bars. -/
syntax "─" : horizontal_border

/-- A top row of a maze. -/
syntax "\n┌" horizontal_border* "┐\n" : game_top_row

/-- A bottom row of a maze. -/
syntax "└" horizontal_border* "┘\n" : game_bottom_row

/-- An empty game cell. -/
syntax "░" : game_cell
/-- A wall game cell. -/
syntax "▓" : game_cell
/-- A player game cell. -/
syntax "@" : game_cell

/-- A row of game cells. -/
syntax "│" game_cell* "│\n" : game_row

/-- A maze is a top row, followed by a sequence of rows, followed by a bottom row. -/
syntax:max game_top_row game_row* game_bottom_row : term

/-- Represents the possible contents of a cell in the maze. -/
inductive CellContents where
  /-- An empty cell. -/
  | empty  : CellContents
  /-- A wall cell. -/
  | wall   : CellContents
  /-- A player cell. -/
  | player : CellContents

/-- Updates the game state by processing a row of cell contents. -/
def update_state_with_row_aux : Nat → Nat → List CellContents → GameState → GameState
|             _,             _, [], oldState => oldState
| currentRowNum, currentColNum, cell::contents, oldState =>
    let oldState' := update_state_with_row_aux currentRowNum (currentColNum+1) contents oldState
    match cell with
    | CellContents.empty => oldState'
    | CellContents.wall => {oldState' .. with
                            walls := ⟨currentColNum,currentRowNum⟩::oldState'.walls}
    | CellContents.player => {oldState' .. with
                              position := ⟨currentColNum,currentRowNum⟩}

/-- Updates the game state by processing a row of cell contents. -/
def update_state_with_row : Nat → List CellContents → GameState → GameState
| currentRowNum, rowContents, oldState => update_state_with_row_aux currentRowNum 0 rowContents oldState

/-- Constructs a game state from a list of cell contents, given the grid size. -/
def game_state_from_cells_aux : Coords → Nat → List (List CellContents) → GameState
| size, _, [] => ⟨size, ⟨0,0⟩, []⟩
| size, currentRow, row::rows =>
        let prevState := game_state_from_cells_aux size (currentRow + 1) rows
        update_state_with_row currentRow row prevState

/-- Constructs a game state from a list of cell contents, given the grid size. -/
def game_state_from_cells : Coords → List (List CellContents) → GameState
| size, cells => game_state_from_cells_aux size 0 cells

/-- Converts a game cell syntax into a term representing its contents. -/
def termOfCell : Lean.TSyntax `game_cell → Lean.MacroM (Lean.TSyntax `term)
| `(game_cell| ░) => `(CellContents.empty)
| `(game_cell| ▓) => `(CellContents.wall)
| `(game_cell| @) => `(CellContents.player)
| _ => Lean.Macro.throwError "unknown game cell"

/-- Converts a game row syntax into a term representing its contents. -/
def termOfGameRow : Nat → Lean.TSyntax `game_row → Lean.MacroM (Lean.TSyntax `term)
| expectedRowSize, `(game_row| │$cells:game_cell*│) =>
      do if cells.size != expectedRowSize
         then Lean.Macro.throwError "row has wrong size"
         let cells' ← Array.mapM termOfCell cells
         `([$cells',*])
| _, _ => Lean.Macro.throwError "unknown game row"

/-- Converts a maze syntax into a term representing its contents. -/
macro_rules
| `(┌$tb:horizontal_border* ┐
    $rows:game_row*
    └ $bb:horizontal_border* ┘) =>
      do let rsize := Lean.Syntax.mkNumLit (toString rows.size)
         let csize := Lean.Syntax.mkNumLit (toString tb.size)
         if tb.size != bb.size then Lean.Macro.throwError "top/bottom border mismatch"
         let rows' ← Array.mapM (termOfGameRow tb.size) rows
         `(game_state_from_cells ⟨$csize,$rsize⟩ [$rows',*])

---------------------------
-- Now we define a delaborator that will cause GameState to be rendered as a maze.

/-- Extracts the coordinates from an expression representing a Coords. -/
def extractXY : Lean.Expr → Lean.MetaM Coords
| e => do
  let e':Lean.Expr ← Lean.Meta.whnf e
  let sizeArgs := Lean.Expr.getAppArgs e'
  let x ← Lean.Meta.whnf sizeArgs[0]!
  let y ← Lean.Meta.whnf sizeArgs[1]!
  let numCols := (Lean.Expr.rawNatLit? x).get!
  let numRows := (Lean.Expr.rawNatLit? y).get!
  return Coords.mk numCols numRows

/-- Extracts the list of wall coordinates from an expression. -/
partial def extractWallList : Lean.Expr → Lean.MetaM (List Coords)
| exp => do
  let exp':Lean.Expr ← Lean.Meta.whnf exp
  let f := Lean.Expr.getAppFn exp'
  if f.constName!.toString == "List.cons"
  then let consArgs := Lean.Expr.getAppArgs exp'
       let rest ← extractWallList consArgs[2]!
       let ⟨wallCol, wallRow⟩ ← extractXY consArgs[1]!
       return (Coords.mk wallCol wallRow) :: rest
  else return [] -- "List.nil"

/-- Extracts a GameState from an expression. -/
partial def extractGameState : Lean.Expr → Lean.MetaM GameState
| exp => do
    let exp': Lean.Expr ← Lean.Meta.whnf exp
    let gameStateArgs := Lean.Expr.getAppArgs exp'
    let size ← extractXY gameStateArgs[0]!
    let playerCoords ← extractXY gameStateArgs[1]!
    let walls ← extractWallList gameStateArgs[2]!
    pure ⟨size, playerCoords, walls⟩

/-- Updates a 2D array at a specific coordinate with a new value. -/
def update2dArray {α : Type} : Array (Array α) → Coords → α → Array (Array α)
| a, ⟨x,y⟩, v =>
   Array.set! a y $ Array.set! a[y]! x v

/-- Updates a 2D array at multiple coordinates with a new value. -/
def update2dArrayMulti {α : Type} : Array (Array α) → List Coords → α → Array (Array α)
| a,    [], _ => a
| a, c::cs, v =>
     let a' := update2dArrayMulti a cs v
     update2dArray a' c v

/-- Delaborates a game row into its syntax representation. -/
def delabGameRow : Array (Lean.TSyntax `game_cell) → Lean.PrettyPrinter.Delaborator.DelabM (Lean.TSyntax `game_row)
| a => `(game_row| │ $a:game_cell* │)

/-- Delaborates a GameState into its syntax representation. -/
def delabGameState : Lean.Expr → Lean.PrettyPrinter.Delaborator.Delab
| e =>
  do guard $ e.getAppNumArgs == 3
     let ⟨⟨numCols, numRows⟩, playerCoords, walls⟩ ←
       try extractGameState e
       catch _ => failure -- can happen if game state has variables in it

     let topBar := Array.replicate numCols $ ← `(horizontal_border| ─)
     let emptyCell ← `(game_cell| ░)

     let a0 := Array.replicate numRows $ Array.replicate numCols emptyCell
     let a1 := update2dArray a0 playerCoords $ ← `(game_cell| @)
     let a2 := update2dArrayMulti a1 walls $ ← `(game_cell| ▓)
     let aa ← Array.mapM delabGameRow a2

     `(┌$topBar:horizontal_border*┐
       $aa:game_row*
       └$topBar:horizontal_border*┘)

/-- The attribute [delab] registers this function as a delaborator for the GameState.mk constructor. -/
@[delab app.GameState.mk] def delabGameStateMk : Lean.PrettyPrinter.Delaborator.Delab := do
  let e ← Lean.PrettyPrinter.Delaborator.SubExpr.getExpr
  delabGameState e

/-- We register the same elaborator for applications of the game_state_from_cells function. -/
@[delab app.game_state_from_cells] def delabGameState' : Lean.PrettyPrinter.Delaborator.Delab :=
  do let e ← Lean.PrettyPrinter.Delaborator.SubExpr.getExpr
     let e' ← Lean.Meta.whnf e
     delabGameState e'

--------------------------

/-- Represents the possible moves in the maze game. -/
inductive Move where
  /-- Move east. -/
  | east  : Move
  /-- Move west. -/
  | west  : Move
  /-- Move north. -/
  | north : Move
  /-- Move south. -/
  | south : Move

/-- Applies a move to the game state, updating the player's position if the move is valid. -/
@[simp]
def make_move : GameState → Move → GameState
| ⟨s, ⟨x,y⟩, w⟩, Move.east =>
             if ! w.elem ⟨x+1, y⟩ ∧ x + 1 ≤ s.x
             then ⟨s, ⟨x+1, y⟩, w⟩
             else ⟨s, ⟨x,y⟩, w⟩
| ⟨s, ⟨x,y⟩, w⟩, Move.west =>
             if ! w.elem ⟨x-1, y⟩
             then ⟨s, ⟨x-1, y⟩, w⟩
             else ⟨s, ⟨x,y⟩, w⟩
| ⟨s, ⟨x,y⟩, w⟩, Move.north =>
             if ! w.elem ⟨x, y-1⟩
             then ⟨s, ⟨x, y-1⟩, w⟩
             else ⟨s, ⟨x,y⟩, w⟩
| ⟨s, ⟨x,y⟩, w⟩, Move.south =>
             if ! w.elem ⟨x, y + 1⟩ ∧ y + 1 ≤ s.y
             then ⟨s, ⟨x, y+1⟩, w⟩
             else ⟨s, ⟨x,y⟩, w⟩

/-- Determines if the current game state is a winning state. -/
def IsWin : GameState → Prop
| ⟨⟨sx, sy⟩, ⟨x,y⟩, _⟩ => x = 0 ∨ y = 0 ∨ x + 1 = sx ∨ y + 1 = sy

/-- Represents whether a game state is escapable. -/
inductive Escapable : GameState → Prop where
  /-- The game is won. -/
| Done (s : GameState) : IsWin s → Escapable s
  /-- The game is not won, but a move can be made. -/
| Step (s : GameState) (m : Move) : Escapable (make_move s m) → Escapable s

/-- If a game state is escapable, then moving west from it is also escapable. -/
theorem step_west
  {s: Coords}
  {x y : Nat}
  {w: List Coords}
  (hclear' : ! w.elem ⟨x,y⟩)
  (W : Escapable ⟨s,⟨x,y⟩,w⟩) :
  Escapable ⟨s,⟨x+1,y⟩, w⟩ := by
    have hmm : GameState.mk s ⟨x,y⟩ w = make_move ⟨s,⟨x+1, y⟩,w⟩ Move.west := by
      have h' : x + 1 - 1 = x := rfl
      simp [h', hclear']
    rw [hmm] at W
    exact .Step ⟨s,⟨x+1,y⟩,w⟩ Move.west W

theorem step_east
  {s: Coords}
  {x y : Nat}
  {w: List Coords}
  (hclear' : ! w.elem ⟨x+1,y⟩)
  (hinbounds : x + 1 ≤ s.x)
  (E : Escapable ⟨s,⟨x+1,y⟩,w⟩) :
  Escapable ⟨s,⟨x, y⟩,w⟩ :=
    by have hmm : GameState.mk s ⟨x+1,y⟩ w = make_move ⟨s, ⟨x,y⟩,w⟩ Move.east :=
         by simp [hclear', hinbounds]
       rw [hmm] at E
       exact .Step ⟨s, ⟨x,y⟩, w⟩ Move.east E

theorem step_north
  {s: Coords}
  {x y : Nat}
  {w: List Coords}
  (hclear' : ! w.elem ⟨x,y⟩)
  (N : Escapable ⟨s,⟨x,y⟩,w⟩) :
  Escapable ⟨s,⟨x, y+1⟩,w⟩ :=
    by have hmm : GameState.mk s ⟨x,y⟩ w = make_move ⟨s,⟨x, y+1⟩,w⟩ Move.north :=
         by have h' : y + 1 - 1 = y := rfl
            simp [h', hclear']
       rw [hmm] at N
       exact .Step ⟨s,⟨x,y+1⟩,w⟩ Move.north N

/-- If a game state is escapable, then moving south from it is also escapable. -/
theorem step_south
  {s: Coords}
  {x y : Nat}
  {w: List Coords}
  (hclear' : ! w.elem ⟨x,y+1⟩)
  (hinbounds : y + 1 ≤ s.y)
  (S : Escapable ⟨s,⟨x,y+1⟩,w⟩) :
  Escapable ⟨s,⟨x, y⟩,w⟩ :=
    by have hmm : GameState.mk s ⟨x,y+1⟩ w = make_move ⟨s,⟨x, y⟩,w⟩ Move.south :=
            by simp [hclear', hinbounds]
       rw [hmm] at S
       exact .Step ⟨s,⟨x,y⟩,w⟩ Move.south S

/-- If a game state is escapable, then moving west from it is also escapable. -/
def escape_west {sx sy : Nat} {y : Nat} {w : List Coords} : Escapable ⟨⟨sx, sy⟩,⟨0, y⟩,w⟩ :=
    .Done _ (Or.inl rfl)

/-- If a game state is escapable, then moving east from it is also escapable. -/
def escape_east {sy x y : Nat} {w : List Coords} : Escapable ⟨⟨x+1, sy⟩,⟨x, y⟩,w⟩ :=
  .Done _ (Or.inr <| Or.inr <| Or.inl rfl)

/-- If a game state is escapable, then moving north from it is also escapable. -/
def escape_north {sx sy : Nat} {x : Nat} {w : List Coords} : Escapable ⟨⟨sx, sy⟩,⟨x, 0⟩,w⟩ :=
  .Done _ (Or.inr <| Or.inl rfl)

/-- If a game state is escapable, then moving south from it is also escapable. -/
def escape_south {sx x y : Nat} {w: List Coords} : Escapable ⟨⟨sx, y+1⟩,⟨x, y⟩,w⟩ :=
  .Done _ (Or.inr <| Or.inr <| Or.inr rfl)

/-- Fail tactic. -/
elab "fail" m:term : tactic => throwError m


/-- Tactic to move west. -/
macro "west" : tactic =>
  `(tactic| first | apply step_west; (· decide; done) | fail "cannot step west")

/-- Tactic to move east. -/
macro "east" : tactic =>
  `(tactic| first | apply step_east; (· decide; done); (· decide; done) | fail "cannot step east")

/-- Tactic to move north. -/
macro "north" : tactic =>
  `(tactic| first | apply step_north; (· decide; done) | fail "cannot step north")

/-- Tactic to move south. -/
macro "south" : tactic =>
  `(tactic| first | apply step_south; (· decide; done); (· decide; done) | fail "cannot step south")

/-- Tactic to escape the maze in any direction. -/
macro "out" : tactic => `(tactic| first | apply escape_north | apply escape_south |
                           apply escape_east | apply escape_west |
                           fail "not currently at maze boundary")

/-- Peephole tactic to strip cancelling opposing directions -/
macro "peephole" : tactic => `(tactic| repeat (first | (north; south) | (south; north) | (east; west) | (west; east)))

/-- Can escape the trivial maze in any direction. -/
example : Escapable ┌─┐
                    │@│
                    └─┘ := by out

/-- Some other mazes with immediate escapes. -/
example : Escapable ┌──┐
                    │░░│
                    │@░│
                    │░░│
                    └──┘ := by out

/-- Can escape the trivial maze in any direction. -/
example : Escapable ┌──┐
                    │░░│
                    │░@│
                    │░░│
                    └──┘ := by out

/-- Can escape the trivial maze in any direction. -/
example : Escapable ┌───┐
                    │░@░│
                    │░░░│
                    │░░░│
                    └───┘ := by out

/-- Can escape the trivial maze in any direction. -/
example : Escapable ┌───┐
                    │░░░│
                    │░░░│
                    │░@░│
                    └───┘ := by out

/-- A maze with an immediate escape. -/
def maze1 := ┌──────┐
             │▓▓▓▓▓▓│
             │▓░░@░▓│
             │▓░░░░▓│
             │▓░░░░▓│
             │▓▓▓▓░▓│
             └──────┘

/-- Can escape the `maze1`. -/
example : Escapable maze1 := by
  west
  west
  east
  south
  south
  east
  east
  south
  out

def maze2 := ┌────────┐
             │▓▓▓▓▓▓▓▓│
             │▓░▓@▓░▓▓│
             │▓░▓░░░▓▓│
             │▓░░▓░▓▓▓│
             │▓▓░▓░▓░░│
             │▓░░░░▓░▓│
             │▓░▓▓▓▓░▓│
             │▓░░░░░░▓│
             │▓▓▓▓▓▓▓▓│
             └────────┘

example : Escapable maze2 :=
 by south
    east
    south
    south
    south
    west
    west
    west
    south
    south
    east
    east
    east
    east
    east
    north
    north
    north
    east
    out

def maze3 := ┌────────────────────────────┐
             │▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓│
             │▓░░░░░░░░░░░░░░░░░░░░▓░░░@░▓│
             │▓░▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓░▓░▓▓▓▓▓│
             │▓░▓░░░▓░░░░▓░░░░░░░░░▓░▓░░░▓│
             │▓░▓░▓░▓░▓▓▓▓░▓▓▓▓▓▓▓▓▓░▓░▓░▓│
             │▓░▓░▓░▓░▓░░░░▓░░░░░░░░░░░▓░▓│
             │▓░▓░▓░▓░▓░▓▓▓▓▓▓▓▓▓▓▓▓░▓▓▓░▓│
             │▓░▓░▓░▓░░░▓░░░░░░░░░░▓░░░▓░▓│
             │▓░▓░▓░▓▓▓░▓░▓▓▓▓▓▓▓▓▓▓░▓░▓░▓│
             │▓░▓░▓░░░░░▓░░░░░░░░░░░░▓░▓░▓│
             │▓░▓░▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓░▓│
             │░░▓░░░░░░░░░░░░░░░░░░░░░░░░▓│
             │▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓│
             └────────────────────────────┘

example : Escapable maze3 :=
 by west
    west
    west
    south
    south
    south
    south
    south
    south
    east
    east
    south
    south
    north
