import LeanPlot.Specification
import LeanPlot.Plot
import Lean.Data.Json

/-! # Mixed Chart Demo

Demonstrates combining different chart types (bars and lines) in a single plot
using ComposedChart.
-/

namespace LeanPlot.Demos

open LeanPlot
open Lean

/-- Sales data as bars with trend line overlay -/
def mixedChartDemo : PlotSpec :=
  -- Create bar chart data for monthly sales
  let months := #[1, 2, 3, 4, 5, 6]
  let sales := #[120, 150, 180, 160, 200, 220]
  let barData := months.zip sales |>.map fun (x, y) =>
    Json.mkObj [("month", toJson x), ("sales", toJson y), ("trend", toJson (100 + 20 * x))]

  {
    chartData := barData,
    series := #[
      -- Bar series for sales
      { name := "Sales", dataKey := "sales", color := "#8884d8", type := "bar" },
      -- Line series for trend
      { name := "Trend", dataKey := "trend", color := "#82ca9d", type := "line" }
    ],
    xAxis := some { dataKey := "month", label := some "Month" },
    yAxis := some { dataKey := "sales", label := some "Amount ($)" },
    title := some "Monthly Sales with Trend Line",
    width := 500,
    height := 300
  }

#plot mixedChartDemo

end LeanPlot.Demos
