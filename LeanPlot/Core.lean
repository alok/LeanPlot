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

/-- Anything that can be coerced to an *interactive* `Html` fragment should
implement this.  We keep it library-local to avoid name clashes with other
`Render` classes in the ecosystem. -/
class Render (α : Type) : Type where
  render : α → Html

export Render (render)

/-- Provide an automatic coercion: terms with a `[Render]` instance can be used
wherever `Html` is expected (e.g. the `#html` / `#plot` commands). -/
instance (α) [Render α] : CoeTC α Html where coe := render

/-! ## Layer and Plot ------------------------------------------------------ -/

/-- *Minimal* information needed to draw a single visual layer.  We expose only
an `html` field for now; later we can add `legend?`, `bounds?`, etc., without
breaking existing code. -/
structure Layer where
  html : Html
  deriving Inhabited

/-- Type-class turning arbitrary user types into `Layer`s.  This is the *open
extension point*: implement `[ToLayer MyFancyPlot]` and you're in. -/
class ToLayer (α : Type) where
  toLayer : α → Layer
export ToLayer (toLayer)

instance : ToLayer Layer where toLayer := id

/-- A *plot* is a bag of layers.  We wrap an `Array` so that extra metadata can
be attached in future (e.g. global scales, titles, facets). -/
structure Plot where
  layers : Array Layer := #[]
  deriving Inhabited

/-- Type-class for things convertible to a `Plot`.  Default rule: if something
is already a `Layer` we lift it into a single-layer plot. -/
class ToPlot (α : Type) where
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

instance : Render Layer where
  render := Layer.html

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
