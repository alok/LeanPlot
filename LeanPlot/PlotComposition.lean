import LeanPlot.Specification
import LeanPlot.Components
import ProofWidgets.Component.HtmlDisplay
import Lean.Data.Json

/-! # LeanPlot.PlotComposition – Advanced plot composition utilities

This module provides utilities for composing plots with different domains and
ensuring proper data alignment when overlaying plots. -/

namespace LeanPlot.PlotComposition

open LeanPlot
open Lean ProofWidgets

/-- Merge two plot specs ensuring proper data alignment.
When overlaying plots with different x-domains, this function resamples
both datasets to a common domain. -/
def mergeAligned (p q : PlotSpec) (steps : Nat := 200) : PlotSpec :=
  -- Extract domain information from both plots
  let getDomain (spec : PlotSpec) : Float × Float :=
    match spec.xAxis with
    | some ax =>
      match ax.domain with
      | some arr =>
        if _h : arr.size >= 2 then
          match arr[0]!.getNum?, arr[1]!.getNum? with
          | Except.ok n1, Except.ok n2 => (n1.toFloat, n2.toFloat)
          | _, _ => (0, 1)
        else (0, 1)
      | none =>
        -- Try to infer from data
        if spec.chartData.isEmpty then (0, 1)
        else
          let xVals := spec.chartData.filterMap fun obj =>
            match obj.getObjVal? "x" with
            | Except.ok xJson =>
              match xJson.getNum? with
              | Except.ok n => some n.toFloat
              | _ => none
            | _ => none
          if xVals.isEmpty then (0, 1)
          else (xVals.foldl min xVals[0]!, xVals.foldl max xVals[0]!)
    | none => (0, 1)

  let (p_min, p_max) := getDomain p
  let (q_min, q_max) := getDomain q

  -- Use the union of domains
  let min_x := min p_min q_min
  let max_x := max p_max q_max

  -- If domains are the same and data aligns, just overlay normally
  if p_min == q_min && p_max == q_max && p.chartData.size == q.chartData.size then
    PlotSpec.overlay p q
  else
    -- Need to resample - for now just concatenate (future: implement resampling)
    PlotSpec.overlay p q

/-- Create a subplot grid from multiple plot specs.
This creates a visual grid layout of multiple plots. -/
def gridLayout (plots : Array PlotSpec) (cols : Nat := 2) : Html :=
  -- React expects `style` props to be an **object** rather than a CSS string (#62).
  -- We therefore build the style as a JSON object so that it is passed through
  -- to the underlying React component correctly.
  let gridStyle : Json := Json.mkObj [
    ("display",             Json.str "grid"),
    ("gridTemplateColumns", Json.str s!"repeat({cols}, 1fr)"),
    ("gap",                 Json.str "10px")
  ]

  let cellStyle : Json := Json.mkObj [
    ("border",   Json.str "1px solid #ddd"),
    ("padding",  Json.str "5px")
  ]

  let cells := plots.map fun plot =>
    Html.element "div" #[("style", cellStyle)]
      #[PlotSpec.render plot]

  Html.element "div" #[("style", gridStyle)] cells

/-- Stack plots vertically with shared x-axis.
Useful for comparing multiple datasets with the same x-domain. -/
def verticalStack (plots : Array PlotSpec) : Html :=
  let stackStyle := Json.str "display: flex; flex-direction: column; gap: 5px;"

  let charts := plots.mapIdx fun i plot =>
    let showXAxis := i == plots.size - 1  -- Only show x-axis on bottom plot
    let adjustedPlot :=
      if showXAxis then plot
      else { plot with xAxis := none }

    Html.element "div" #[] #[PlotSpec.render adjustedPlot]

  Html.element "div" #[("style", stackStyle)] charts

/-- Normalize multiple plots to the same y-scale for fair comparison. -/
def normalizeYScale (plots : Array PlotSpec) : Array PlotSpec :=
  -- Find global min/max across all plots
  let getYRange (spec : PlotSpec) : Option (Float × Float) :=
    if spec.chartData.isEmpty then none
    else
      let yKeys := spec.series.map (·.dataKey)
      let yVals := spec.chartData.flatMap fun obj =>
        yKeys.flatMap fun key =>
          match obj.getObjVal? key with
          | Except.ok yJson =>
            match yJson.getNum? with
            | Except.ok n => #[n.toFloat]
            | _ => #[]
          | _ => #[]
      if yVals.isEmpty then none
      else some (yVals.foldl min yVals[0]!, yVals.foldl max yVals[0]!)

  let ranges := plots.filterMap getYRange
  if ranges.isEmpty then plots
  else
    let globalMin := ranges.map (·.1) |>.foldl min ranges[0]!.1
    let globalMax := ranges.map (·.2) |>.foldl max ranges[0]!.2

    plots.map fun plot =>
      PlotSpec.withYDomain plot globalMin globalMax

/-- Apply a consistent color scheme across multiple plots. -/
def applyColorScheme (plots : Array PlotSpec) (palette : Array String) : Array PlotSpec :=
  let totalSeries := plots.foldl (fun acc p => acc + p.series.size) 0
  let colors := if palette.size >= totalSeries then palette
    else palette ++ (List.range (totalSeries - palette.size)).toArray.map
      (fun i => LeanPlot.Palette.colorFromNat (palette.size + i))

  -- Use a fold to track color index across plots
  let (_, result) := plots.foldl (fun (colorIdx, acc) plot =>
    let (newColorIdx, newSeries) := plot.series.foldl (fun (idx, seriesAcc) s =>
      let color := colors[idx]!
      (idx + 1, seriesAcc.push { s with color := color })
    ) (colorIdx, #[])
    (newColorIdx, acc.push { plot with series := newSeries })
  ) (0, #[])

  result

end LeanPlot.PlotComposition
