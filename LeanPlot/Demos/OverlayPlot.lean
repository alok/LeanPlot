import LeanPlot.Components

open Lean ProofWidgets Recharts LeanPlot.Components
open scoped ProofWidgets.Jsx

namespace LeanPlot.Demos

/--
Sample a function on `[0,1]` uniformly and return Recharts-friendly JSON.
-/
private def samplePair (steps : Nat := 200) : Array Json :=
  (List.range (steps + 1)).toArray.map (fun i =>
    let x : Float := i.toFloat / steps.toFloat
    let y1 := x
    let y2 := x * x
    json% {x: $(toJson x), y1: $(toJson y1), y2: $(toJson y2)})

/-- Interactive overlay of `y = x` and `y = x²`. Put the cursor on the line to render. -/
def overlay : Html :=
  let data := sampleMany #[("y1", fun x => x), ("y2", fun x => x * x)] 200 0 1
  mkLineChart data #[("y1", "#1f77b4"), ("y2", "#ff7f0e")] 400 400

#html overlay

end LeanPlot.Demos
