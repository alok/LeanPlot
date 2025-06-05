import LeanPlot.Series
import LeanPlot.Specification
import LeanPlot.Plot
import Lean

/-! # SeriesKind Demo

Demonstrates the type-safe dependent series specification system.
This shows how the new SeriesKind enum and dependent types provide
compile-time guarantees about series configuration.
-/

namespace LeanPlot.Demos

open LeanPlot
open Lean

/-- Create a line series using the dependent type system -/
def typeSafeLine : SeriesDSpecPacked :=
  { kind := .line
    spec := {
      name := "Temperature"
      dataKey := "temp"
      details := .line {
        color := "#ff6b6b"
        dot := false
      }
    }
  }

/-- Create a scatter series using the dependent type system -/
def typeSafeScatter : SeriesDSpecPacked :=
  { kind := .scatter
    spec := {
      name := "Observations"
      dataKey := "obs"
      details := .scatter {
        color := "#4ecdc4"
        shape := "circle"
      }
    }
  }

/-- Create a bar series using the dependent type system -/
def typeSafeBar : SeriesDSpecPacked :=
  { kind := .bar
    spec := {
      name := "Sales"
      dataKey := "sales"
      details := .bar {
        color := "#45aaf2"
      }
    }
  }

/-- Create an area series using the dependent type system -/
def typeSafeArea : SeriesDSpecPacked :=
  { kind := .area
    spec := {
      name := "Coverage"
      dataKey := "coverage"
      details := .area {
        fill := "#a55eea"
        stroke := "#8854d0"
      }
    }
  }

/-- Demonstrate type-safe rendering -/
def renderingDemo : PlotSpec :=
  -- Create some sample data
  let data : Array Json := #[
    json% { x: 1, temp: 20, obs: 22, sales: 100, coverage: 50 },
    json% { x: 2, temp: 22, obs: 21, sales: 120, coverage: 60 },
    json% { x: 3, temp: 25, obs: 26, sales: 110, coverage: 75 },
    json% { x: 4, temp: 23, obs: 24, sales: 140, coverage: 80 }
  ]

  -- Convert our type-safe series to legacy format for now
  -- (until PlotSpec is updated to use SeriesDSpec directly)
  let series := #[
    typeSafeLine.toLayerSpec,
    typeSafeScatter.toLayerSpec,
    typeSafeBar.toLayerSpec,
    typeSafeArea.toLayerSpec
  ]

  { chartData := data
    series := series
    title := some "Type-Safe Series Demo"
    width := 600
    height := 400
    legend := true
  }

#plot renderingDemo

/-- Example showing the type safety of the dependent system.
The details field must match the kind, enforced at compile time. -/
example : SeriesDSpec SeriesKind.line :=
  { name := "Temperature"
    dataKey := "temp"
    details := .line { color := "#ff0000", dot := true } }

-- This would be a compile error:
-- example : SeriesDSpec SeriesKind.line :=
--   { name := "Bad"
--     dataKey := "bad"
--     details := .scatter { color := "#ff0000" } }  -- Type mismatch!

end LeanPlot.Demos
