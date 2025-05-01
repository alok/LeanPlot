import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Recharts

open Lean ProofWidgets Recharts
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
  let data := samplePair 200;
  <LineChart width={400} height={400} data={data}>
    <XAxis domain?={#[toJson 0, toJson 1]} dataKey?="x" />
    <YAxis domain?={#[toJson 0, toJson 1]} />
    <Line type={.monotone} dataKey="y1" stroke="#1f77b4" dot?={Bool.false} />
    <Line type={.monotone} dataKey="y2" stroke="#ff7f0e" dot?={Bool.false} />
  </LineChart>

#html overlay

end LeanPlot.Demos
