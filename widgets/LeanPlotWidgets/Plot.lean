import LeanPlotWidgets.Figure

/-!
# `#plot`: one-line line plots in the infoview

```lean
#plot Float.sin                               -- sampled on -5..5 (201 points)
#plot (fun x => x * Float.exp (-x)) on 0..8   -- explicit domain
#plot Float.tanh on -3..3 using 50            -- and sample count
#plot #[1.0, 4.0, 9.0, 16.0]                  -- data against 1, 2, …, n
#plot [Float.sin, Float.cos] on 0..6.3        -- several functions, palette colours
```

`#plot e` is Makie's non-mutating `lines`: one `Axis2` with Makie's defaults in a default
`Figure` (600 × 450), shown like `#figure`. What is drawn depends on the type of `e` (the
`Plottable` class):

| `e` | Makie counterpart |
|---|---|
| `Float → Float` | `lines(range(a, b, length = n), f)` |
| `Float → Float × Float` | `lines(Point2.(f.(range(a, b, length = n))))` (parametric) |
| `Array`/`List` of `Float → Float` | one `lines!` per function |
| `FloatArray`, `Array`/`List` of `Float`, `Nat`, `Int` | `lines(ys)` (x = 1, 2, …, n) |
| `Pts2`, pairs of arrays, arrays of pairs | `lines(xs, ys)` |
| `Figure`, `Scene`, `Axis2`, `Axis3` | shown as is (as `#figure`) |

Functions are first elaborated as `Float → Float`, so `fun x => x * x` needs no type
ascription. The domain defaults to `-5..5`, the range Makie's (and Plots.jl's) `tryrange`
starts from for a function plotted without one. Makie samples `lines(a..b, f)` adaptively
(`PlotUtils.adapted_grid`, with a seeded random jitter); `#plot` instead samples Julia's
`range(a, b, length = n)` exactly (`Num.range`), which is deterministic and matches
`lines(range(a, b, length = n), f)` point for point.

Syntax notes: write the domain without spaces around `..` (`-1..1`, as in Mathlib's
`∫ x in a..b`), because `a ..` with a space parses as an application to `..`. With `on` or
`using`, the plotted term must be an identifier, a literal or parenthesized:
`#plot (fun x => x * x) on 0..1`.
-/

namespace LeanPlot.Widgets

open Lean Elab Command Term Meta

/-- Sampling of `#plot` for functions. -/
structure PlotSpec where
  /-- The interval `(a, b)` of `on a..b`. -/
  domain : Float × Float := (-5, 5)
  /-- The number of samples `n` of `using n` (Julia `range(a, b, length = n)`). -/
  samples : Nat := 201
  deriving Repr, Inhabited

namespace PlotSpec

/-- The sample points `range(a, b, length = n)`. -/
def xs (s : PlotSpec) : FloatArray := Num.range s.domain.1 s.domain.2 s.samples

/-- Why the spec cannot be sampled, if it cannot. -/
def check (s : PlotSpec) : Option String :=
  let (a, b) := s.domain
  let num (x : Float) : String := if x.isFinite then Num.fmtFixed 6 x else toString x
  let dom := s!"{num a}..{num b}"
  if !(a.isFinite && b.isFinite) then some s!"#plot: the domain {dom} is not finite"
  else if a == b then some s!"#plot: the domain {dom} is empty"
  else if s.samples < 2 then some s!"#plot: need at least 2 samples, got {s.samples}"
  else none

end PlotSpec

/-- Values `#plot` can draw into a fresh `Axis2`. -/
class Plottable (α : Type) where
  /-- Add the plot(s) of the value to an axis. -/
  plot : α → PlotSpec → Axis2 → Axis2

namespace Plottable

instance : Plottable (Float → Float) := ⟨fun f s ax => ax.linesFn f s.xs⟩

instance : Plottable (Float → Float × Float) := ⟨fun f s ax =>
  let ts := s.xs
  let rec go (i : Nat) (xs ys : FloatArray) : FloatArray × FloatArray :=
    if h : i < ts.size then
      let (x, y) := f ts[i]
      go (i + 1) (xs.push x) (ys.push y)
    else (xs, ys)
  termination_by ts.size - i
  let (xs, ys) := go 0 (FloatArray.emptyWithCapacity ts.size) (FloatArray.emptyWithCapacity ts.size)
  ax.lines xs ys⟩

instance : Plottable (Array (Float → Float)) := ⟨fun fs s ax =>
  let xs := s.xs
  fs.foldl (init := ax) fun ax f => ax.linesFn f xs⟩

instance : Plottable (List (Float → Float)) := ⟨fun fs => Plottable.plot fs.toArray⟩

instance : Plottable Pts2 := ⟨fun p _ ax => ax.linesPts p⟩

instance : Plottable FloatArray := ⟨fun ys _ ax => ax.lines (Recipes.oneTo ys.size) ys⟩
instance : Plottable (Array Float) := ⟨fun ys => Plottable.plot (FloatArray.mk ys)⟩
instance : Plottable (List Float) := ⟨fun ys => Plottable.plot ys.toArray⟩
instance : Plottable (Array Nat) := ⟨fun ys => Plottable.plot (ys.map Nat.toFloat)⟩
instance : Plottable (List Nat) := ⟨fun ys => Plottable.plot (ys.toArray.map Nat.toFloat)⟩
instance : Plottable (Array Int) := ⟨fun ys => Plottable.plot (ys.map Float.ofInt)⟩
instance : Plottable (List Int) := ⟨fun ys => Plottable.plot (ys.toArray.map Float.ofInt)⟩

instance : Plottable (FloatArray × FloatArray) := ⟨fun (xs, ys) _ ax => ax.lines xs ys⟩
instance : Plottable (Array Float × Array Float) := ⟨fun (xs, ys) _ ax => ax.lines ⟨xs⟩ ⟨ys⟩⟩
instance : Plottable (Array (Float × Float)) := ⟨fun ps _ ax => ax.lines ⟨ps.map (·.1)⟩ ⟨ps.map (·.2)⟩⟩
instance : Plottable (List (Float × Float)) := ⟨fun ps => Plottable.plot ps.toArray⟩

end Plottable

/-- The figure `#plot a` shows: one default `Axis2` in a default `Figure`. -/
def plotFigure {α : Type} [Plottable α] (a : α) (spec : PlotSpec := {}) : Figure :=
  Figure.new |>.axis 1 1 (Plottable.plot a spec Axis2.new)

/-- The scene `#plot a` shows; fails on a degenerate `spec`. -/
def plotScene {α : Type} [Plottable α] (a : α) (spec : PlotSpec) : IO Scene := do
  if let some err := spec.check then throw (IO.userError err)
  return (plotFigure a spec).toScene

/-- `on a..b`: the domain of `#plot`. Write it without spaces around `..`. -/
syntax plotDomain := &" on " term ".." term

/-- `using n`: the number of samples of `#plot`. -/
syntax plotSamples := " using " term

/-- Plot a function or data in the infoview: `#plot e`, `#plot f on a..b`,
`#plot f on a..b using n`, `#plot f using n`. A doc comment becomes the caption. -/
syntax (name := plotCmd) (docComment)? "#plot " term : command

@[inherit_doc plotCmd]
syntax (name := plotOnCmd) (docComment)? "#plot " term:max plotDomain (plotSamples)? : command

@[inherit_doc plotCmd]
syntax (name := plotUsingCmd) (docComment)? "#plot " term:max plotSamples : command

/-- The `PlotSpec` term of the optional `on a..b` and `using n` clauses. -/
def specSyntax (dom? : Option (Term × Term)) (n? : Option Term) : MacroM Term :=
  match dom?, n? with
  | none, none => `(({} : LeanPlot.Widgets.PlotSpec))
  | some (a, b), none => `(({ domain := ($a, $b) } : LeanPlot.Widgets.PlotSpec))
  | none, some n => `(({ samples := $n } : LeanPlot.Widgets.PlotSpec))
  | some (a, b), some n => `(({ domain := ($a, $b), samples := $n } : LeanPlot.Widgets.PlotSpec))

/-- Whether `#plot` samples values of type `ty`: functions, and arrays or lists of functions. -/
def isSampled (ty : Expr) : MetaM Bool := do
  let ty ← whnfR ty
  if ty.isForall then return true
  if ty.isAppOfArity ``Array 1 || ty.isAppOfArity ``List 1 then
    return (← whnfR ty.appArg!).isForall
  return false

/-- The `IO Scene` expression `#plot` evaluates. `hasSpec`: an `on`/`using` clause was given
(it only applies to functions). -/
def plotSceneExpr (e : Term) (specStx : Term) (hasSpec : Bool) : TermElabM Expr := do
  let spec ← withoutErrToSorry do
    let s ← elabTermEnsuringType specStx (mkConst ``PlotSpec)
    synthesizeSyntheticMVarsNoPostponing
    instantiateMVars s
  let floatFn ← mkArrow (mkConst ``Float) (mkConst ``Float)
  let v ← elabFirst e [some floatFn, none]
  ensureEvaluable "#plot" e v
  let ty ← instantiateMVars (← inferType v)
  if let some inst ← synthInstance? (mkApp (mkConst ``Plottable) ty) then
    if hasSpec && !(← isSampled ty) then
      logWarningAt e m!"#plot: `on`/`using` only apply to functions; ignored for{indentExpr ty}"
    return mkApp4 (mkConst ``plotScene) ty inst v spec
  if let some x ← figureEvalExpr? v then
    if hasSpec then logWarningAt e m!"#plot: `on`/`using` only apply to functions; ignored for{indentExpr ty}"
    return x
  throwErrorAt e m!"#plot: cannot plot a value of type{indentExpr ty}\n\
    expected a function `Float → Float` (or `Float → Float × Float`), a list or array of them, \
    data (`FloatArray`, arrays or lists of numbers or pairs, `Pts2`), or a Figure, Scene, Axis2 or Axis3"

/-- Shared elaborator of the `#plot` forms. -/
def elabPlotCore (stx : Syntax) (doc? : Option (TSyntax ``Lean.Parser.Command.docComment)) (e : Term)
    (dom? : Option (Term × Term)) (n? : Option Term) : CommandElabM Unit := do
  let caption ← captionOf doc?
  let specStx ← liftMacroM <| specSyntax dom? n?
  let sceneIO ← liftTermElabM <| plotSceneExpr e specStx (dom?.isSome || n?.isSome)
  showScene stx sceneIO caption

@[command_elab plotCmd]
def elabPlot : CommandElab := fun stx => do
  match stx with
  | `($[$doc?:docComment]? #plot $e:term) => elabPlotCore stx doc? e none none
  | _ => throwUnsupportedSyntax

@[command_elab plotOnCmd]
def elabPlotOn : CommandElab := fun stx => do
  match stx with
  | `($[$doc?:docComment]? #plot $e:term on $a..$b $[using $n?]?) => elabPlotCore stx doc? e (some (a, b)) n?
  | _ => throwUnsupportedSyntax

@[command_elab plotUsingCmd]
def elabPlotUsing : CommandElab := fun stx => do
  match stx with
  | `($[$doc?:docComment]? #plot $e:term using $n) => elabPlotCore stx doc? e none (some n)
  | _ => throwUnsupportedSyntax

end LeanPlot.Widgets
