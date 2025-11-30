import LeanPlot.SVG

/-!
# Generate Documentation Images

Run with: lake exe gendocimages
Outputs SVG files to doc/img/
-/

open LeanPlot.SVG

/-- Pi constant -/
def Float.pi : Float := 3.14159265358979323846

/-- Generate documentation images -/
def main : IO Unit := do
  IO.println "Generating documentation images..."

  -- Create output directory
  IO.FS.createDirAll "doc/img"

  -- 1. Simple quadratic plot
  let quadratic := sampleFn (fun x => x * x)
  let svg1 := lineChartSVG quadratic "#plot (fun x => x^2)" "#2563eb"
  IO.FS.writeFile "doc/img/plot_quadratic.svg" svg1
  IO.println "  ✓ plot_quadratic.svg"

  -- 2. Sin/Cos comparison
  let sinData := sampleFn (fun x => Float.sin (x * 2 * Float.pi)) 100
  let cosData := sampleFn (fun x => Float.cos (x * 2 * Float.pi)) 100
  let svg2 := multiLineChartSVG #[
    ("sin", sinData, "#2563eb"),
    ("cos", cosData, "#dc2626")
  ] "plotMany sin/cos"
  IO.FS.writeFile "doc/img/plot_sincos.svg" svg2
  IO.println "  ✓ plot_sincos.svg"

  -- 3. Scatter plot (with some noise simulation using deterministic pattern)
  let scatterData := (List.range 50).toArray.map fun i =>
    let x := i.toFloat / 50.0
    let noise := Float.sin (i.toFloat * 17.0) * 0.05  -- Deterministic "noise"
    (x, x * x + noise)
  let svg3 := scatterChartSVG scatterData "scatter plot" "#059669"
  IO.FS.writeFile "doc/img/scatter_demo.svg" svg3
  IO.println "  ✓ scatter_demo.svg"

  -- 4. Exponential decay with oscillation
  let expDecay := sampleFn (fun x => Float.exp (-3 * x) * Float.cos (10 * x)) 200
  let svg4 := lineChartSVG expDecay "damped oscillation" "#7c3aed"
  IO.FS.writeFile "doc/img/plot_damped.svg" svg4
  IO.println "  ✓ plot_damped.svg"

  -- 5. Tanh function
  let tanhData := sampleFn (fun x => Float.tanh (5 * (x - 0.5))) 100
  let svg5 := lineChartSVG tanhData "tanh activation" "#ea580c"
  IO.FS.writeFile "doc/img/plot_tanh.svg" svg5
  IO.println "  ✓ plot_tanh.svg"

  -- 6. Cubic function
  let cubic := sampleFn (fun x => (x - 0.5) * (x - 0.5) * (x - 0.5) * 8) 100
  let svg6 := lineChartSVG cubic "cubic function" "#0891b2"
  IO.FS.writeFile "doc/img/plot_cubic.svg" svg6
  IO.println "  ✓ plot_cubic.svg"

  IO.println "\nGenerated 6 SVG images in doc/img/"
