import LeanPlot.Graphic
import LeanPlot.Render.Export

/-!
# Test PNG/SVG Export

Quick test to verify the export functionality works.
-/

set_option linter.missingDocs false

open LeanPlot

def main : IO Unit := do
  -- Create a simple sine wave plot
  let g := plot (fun x => Float.sin x)
    |>.domain (-3.14159) 3.14159
    |>.samples 100

  -- Save as SVG
  IO.println "Saving SVG..."
  g.saveSVG "test_plot.svg"
  IO.println "✓ Saved test_plot.svg"

  -- Save as PNG
  IO.println "Saving PNG..."
  g.savePNG "test_plot.png"
  IO.println "✓ Saved test_plot.png"

  -- Test overlay
  let g2 := (plot (fun x => Float.sin x) + plot (fun x => Float.cos x))
    |>.domain (-3.14159) 3.14159
    |>.title "Sin and Cos"

  g2.saveSVG "test_overlay.svg"
  IO.println "✓ Saved test_overlay.svg"

  IO.println "All exports completed!"
