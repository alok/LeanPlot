import LeanPlot.API
import LeanPlot.Debug

open LeanPlot.API
open LeanPlot.Debug

/--
Open this file with the Lean LSP (e.g., VS Code) and render the widgets in
the infoview. Click the Save PNG buttons to download images. This lives in a
separate Lake package to keep the core library clean.
-/

#html withSavePNG (plot (fun x => x^2)) "png-demo-quad" "png_demo_quadratic.png"
#html withSavePNG (plotMany #[("sin", fun x => Float.sin x), ("cos", fun x => Float.cos x)])
  "png-demo-trig" "png_demo_trig.png"

