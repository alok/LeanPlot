/-! # Brainfuck DSL with Peephole Optimization

This module implements a Brainfuck interpreter in Lean 4 with:
- A DSL for writing Brainfuck programs
- An AST representation
- Peephole optimizations
- An interpreter
-/

namespace Brainfuck

/-- Brainfuck instructions AST -/
inductive Inst where
  | right (n : Nat := 1)     -- > Move pointer right
  | left (n : Nat := 1)      -- < Move pointer left
  | inc (n : Nat := 1)       -- + Increment cell
  | dec (n : Nat := 1)       -- - Decrement cell
  | output                   -- . Output cell
  | input                    -- , Input to cell
  | loop (body : List Inst)  -- [...] Loop while cell != 0
  -- Optimized instructions
  | setZero                  -- [-] Set cell to 0
  | moveRight                -- [->+<] Move cell right
  | moveLeft                 -- [-<+>] Move cell left
  deriving Repr, BEq

/-- Brainfuck program state -/
structure State where
  memory : Array UInt8
  ptr : Nat
  input : List UInt8
  output : List UInt8
  deriving Repr

/-- Initial state with 30000 cells -/
def State.init (input : List UInt8 := []) : State :=
  { memory := Array.mkArray 30000 0
  , ptr := 0
  , input := input
  , output := [] }

/-- Get current cell value -/
def State.current (s : State) : UInt8 :=
  s.memory.get! s.ptr

/-- Set current cell value -/
def State.setCurrent (s : State) (val : UInt8) : State :=
  { s with memory := s.memory.set! s.ptr val }

/-- Move pointer safely -/
def State.movePtr (s : State) (delta : Int) : State :=
  let newPtr := Int.ofNat s.ptr + delta
  if newPtr >= 0 && newPtr < Int.ofNat s.memory.size then
    { s with ptr := newPtr.natAbs }
  else s

/-- Peephole optimization -/
partial def optimize : List Inst → List Inst
  | [] => []
  | Inst.loop [Inst.dec 1] :: rest =>
    -- [-] → setZero
    Inst.setZero :: optimize rest
  | Inst.loop [Inst.dec 1, Inst.right 1, Inst.inc 1, Inst.left 1] :: rest =>
    -- [->+<] → moveRight
    Inst.moveRight :: optimize rest
  | Inst.right n :: Inst.right m :: rest =>
    -- Merge consecutive rights
    optimize (Inst.right (n + m) :: rest)
  | Inst.left n :: Inst.left m :: rest =>
    -- Merge consecutive lefts
    optimize (Inst.left (n + m) :: rest)
  | Inst.inc n :: Inst.inc m :: rest =>
    -- Merge consecutive increments
    optimize (Inst.inc (n + m) :: rest)
  | Inst.dec n :: Inst.dec m :: rest =>
    -- Merge consecutive decrements
    optimize (Inst.dec (n + m) :: rest)
  | Inst.loop body :: rest =>
    -- Optimize loop body recursively
    Inst.loop (optimize body) :: optimize rest
  | inst :: rest =>
    inst :: optimize rest

/-- Execute instruction -/
partial def execute (inst : Inst) (s : State) : State :=
  match inst with
  | Inst.right n => s.movePtr (Int.ofNat n)
  | Inst.left n => s.movePtr (-(Int.ofNat n))
  | Inst.inc n => s.setCurrent (s.current + n.toUInt8)
  | Inst.dec n => s.setCurrent (s.current - n.toUInt8)
  | Inst.output => { s with output := s.output ++ [s.current] }
  | Inst.input =>
    match s.input with
    | [] => s.setCurrent 0
    | c :: rest => { s.setCurrent c with input := rest }
  | Inst.loop body =>
    let rec execLoop (s : State) : State :=
      if s.current = 0 then s
      else execLoop (body.foldl (fun s inst => execute inst s) s)
    execLoop s
  | Inst.setZero => s.setCurrent 0
  | Inst.moveRight =>
    let val := s.current
    let s' := s.setCurrent 0
    let s'' := s'.movePtr 1
    let s''' := s''.setCurrent (s''.current + val)
    s'''.movePtr (-1)  -- Move back to original position
  | Inst.moveLeft =>
    let val := s.current
    let s' := s.setCurrent 0
    let s'' := s'.movePtr (-1)
    let s''' := s''.setCurrent (s''.current + val)
    s'''.movePtr 1  -- Move back to original position

/-- Run a Brainfuck program -/
def run (program : List Inst) (input : String := "") : String :=
  let inputBytes := input.toList.map (·.toNat.toUInt8)
  let initial := State.init inputBytes
  let optimized := optimize program
  let final := optimized.foldl (fun s inst => execute inst s) initial
  String.mk (final.output.map (fun b => Char.ofNat b.toNat))

/-! ## Examples -/

open Inst

-- Hello World
def helloWorld : List Inst := [
  inc 10,
  loop [
    right, inc 7,
    right, inc 10,
    right, inc 3,
    right, inc 1,
    left 4, dec
  ],
  right, inc 2, output,     -- H
  right, inc 1, output,     -- e
  inc 7, output, output,    -- ll
  inc 3, output,            -- o
  right, inc 2, output,     -- space
  left 2, inc 15, output,   -- W
  right, output,            -- o
  inc 3, output,            -- r
  dec 6, output,            -- l
  dec 8, output,            -- d
  right, inc 1, output      -- !
]

-- Add two digits
def addDigits : List Inst := [
  input,                    -- Read first digit
  dec 48,                   -- Convert from ASCII
  right,
  input,                    -- Read second digit
  dec 48,                   -- Convert from ASCII
  left,
  loop [dec, right, inc, left],  -- Add first to second
  right,
  inc 48,                   -- Convert back to ASCII
  output                    -- Output result
]

-- Test optimization patterns
def testOptimization : List Inst := [
  inc 5, inc 3,            -- Will merge to inc 8
  loop [dec],              -- Will become setZero
  right, right,            -- Will merge to right 2
  inc 10,
  loop [dec, right, inc, left]  -- Will become moveRight
]

#eval run helloWorld
#eval run addDigits "2"  -- Should output "7"
#eval optimize testOptimization

end Brainfuck
