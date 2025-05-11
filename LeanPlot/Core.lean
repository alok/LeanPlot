import ProofWidgets.Component.HtmlDisplay
import LeanPlot.Constants
import LeanPlot.Palette
import LeanPlot.Components

/-! # LeanPlot.Core – extensible plot layer abstraction

Provides the *open-world* foundations promised by the "algebra of graphics"
conversation: any user-defined type that implements `[ToLayer]` or `[ToPlot]`
can seamlessly participate in LeanPlot composition and render via `#plot`.
-/

open Lean ProofWidgets
open scoped ProofWidgets.Jsx

/-- A structure that can be rendered as HTML. -/
structure Renderable where
  /-- A function that renders the structure as HTML. -/
  render : Html

/-- A typeclass for structures that can be rendered as HTML. -/
class Render (α : Type u) where
  /-- A function that renders an instance of the type as HTML. -/
  render : α → Html

export Render (render)

/-- Provide an automatic coercion: terms with a `[Render]` instance can be used
wherever `Html` is expected (e.g. the `#html` / `#plot` commands). -/
instance (α) [Render α] : CoeTC α Html where coe := render

/-! ## Layer and Plot ------------------------------------------------------ -/

/-- A plot layer. -/
structure Layer where
  /-- The HTML representation of the layer. -/
  html : Html
  deriving Inhabited

/-- A typeclass for structures that can be converted to a plot layer. -/
class ToLayer (α : Type u) where
  /-- Converts an instance of the type to a plot layer. -/
  toLayer : α → Layer
export ToLayer (toLayer)

instance : ToLayer Layer where toLayer := id

/-- A plot, which is a collection of layers. -/
structure Plot where
  /-- The layers of the plot. -/
  layers : Array Layer := #[]
  deriving Inhabited

/-- A typeclass for structures that can be converted to a plot. -/
class ToPlot (α : Type u) where
  /-- Converts an instance of the type to a plot. -/
  toPlot : α → Plot
export ToPlot (toPlot)

instance : ToPlot Plot where toPlot := id
instance [ToLayer α] : ToPlot α where
  toPlot a := ⟨#[toLayer a]⟩

/-- Concatenate the layer arrays.  This gives us an associative overlay
operator out of the box. -/
@[inline] def Plot.overlay (p q : Plot) : Plot := ⟨p.layers ++ q.layers⟩

/-- `+` overlays two plots. -/
instance : HAdd Plot Plot Plot where hAdd := Plot.overlay

/-- Overlay any two values that can be coerced to `Plot` via `[ToPlot]`.
The result is a `Plot` containing all layers from both operands. -/
instance (priority := 2000) [ToPlot α] [ToPlot β] : HAdd α β Plot where
  hAdd a b := Plot.overlay (toPlot a) (toPlot b)

/-- Generic `*` overlay via `[ToPlot]`. -/
instance (priority := 2000) [ToPlot α] [ToPlot β] : HMul α β Plot where
  hMul a b := Plot.overlay (toPlot a) (toPlot b)

/-- Generic `/` overlay via `[ToPlot]`.  Provided for API symmetry. -/
instance (priority := 2000) [ToPlot α] [ToPlot β] : HDiv α β Plot where
  hDiv a b := Plot.overlay (toPlot a) (toPlot b)

/-! ### Render instance ---------------------------------------------------- -/

instance : Render Renderable where
  render r := r.render

instance : Render Layer where
  render l := l.html

/-- Render a plot by overlaying all layers in a single relative container.
For line/scatter overlays we ideally want a *single* combined Recharts chart;
that is a future optimisation. -/
instance : Render Plot where
  render p :=
    let rows := (List.range p.layers.size).toArray.map (fun idx =>
      let l := p.layers[idx]!
      Html.element "div"
        #[("key", Json.str (toString idx))]
        #[l.html])
    Html.element "div"
      #[("style", Json.str "display:flex; flex-direction:column;")]
      rows
