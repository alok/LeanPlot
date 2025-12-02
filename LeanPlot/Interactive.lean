import Lean
import LeanPlot.Graphic
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Panel.Basic
import ProofWidgets.Component.Recharts

/-!
# LeanPlot.Interactive - Two-Way Binding Slider Widgets

This module provides interactive sliders that can modify plot parameters
directly in the source code via the VS Code API.

## Key Features

- **Domain sliders**: Adjust the x-axis range with instant visual feedback
- **Samples slider**: Control sampling resolution
- **Source rewriting**: Slider changes are committed to source code via `applyEdit`

## Example Usage

```
-- The #iplot command shows the plot with interactive sliders
#iplot (fun x => x^2) domain=(-2, 2) samples=100
```

When you drag the domain or samples slider, the source code is automatically
updated to reflect the new values.
-/

open Lean ProofWidgets ProofWidgets.Recharts Widget Server Elab Command
open scoped ProofWidgets.Jsx

namespace LeanPlot.Interactive

/-! ## Interactive Plot Props -/

/-- Props for the interactive plot widget -/
structure InteractivePlotProps where
  /-- Sampled data points as JSON array -/
  data : Json
  /-- Current domain lower bound -/
  domainLo : Float
  /-- Current domain upper bound -/
  domainHi : Float
  /-- Current number of samples -/
  samples : Nat
  /-- Line number where the command is (0-indexed) -/
  lineNum : Nat
  /-- Document URI for edits -/
  uri : String
  /-- Chart width -/
  width : Nat
  /-- Chart height -/
  height : Nat
  /-- Series color -/
  color : String
  deriving FromJson, ToJson, Inhabited

/-! ## Interactive Plot Widget -/

/-- The interactive plot panel widget with sliders -/
@[widget_module]
def InteractivePlotPanel : Component InteractivePlotProps where
  javascript := "
import * as React from 'react';
import { EditorContext } from '@leanprover/infoview';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, ResponsiveContainer, Tooltip } from 'recharts';
const e = React.createElement;

export default function(props) {
  const ec = React.useContext(EditorContext);
  const [domainLo, setDomainLo] = React.useState(props.domainLo);
  const [domainHi, setDomainHi] = React.useState(props.domainHi);
  const [samples, setSamples] = React.useState(props.samples);
  const commitTimeoutRef = React.useRef(null);

  // Debounced commit to source
  const commitToSource = (lo, hi, n) => {
    if (commitTimeoutRef.current) {
      clearTimeout(commitTimeoutRef.current);
    }
    commitTimeoutRef.current = setTimeout(() => {
      const loStr = Number.isInteger(lo) ? lo.toString() : lo.toFixed(2);
      const hiStr = Number.isInteger(hi) ? hi.toString() : hi.toFixed(2);
      const newText = `#iplot fun x => x domain=(${loStr}, ${hiStr}) samples=${n}`;
      ec.api.applyEdit({
        documentChanges: [{
          textDocument: { uri: props.uri, version: null },
          edits: [{
            range: {
              start: { line: props.lineNum, character: 0 },
              end: { line: props.lineNum, character: 1000 }
            },
            newText: newText
          }]
        }]
      });
    }, 150);
  };

  const handleDomainLoChange = (e) => {
    const val = parseFloat(e.target.value);
    setDomainLo(val);
    if (val < domainHi) {
      commitToSource(val, domainHi, samples);
    }
  };

  const handleDomainHiChange = (e) => {
    const val = parseFloat(e.target.value);
    setDomainHi(val);
    if (val > domainLo) {
      commitToSource(domainLo, val, samples);
    }
  };

  const handleSamplesChange = (e) => {
    const val = parseInt(e.target.value);
    setSamples(val);
    commitToSource(domainLo, domainHi, val);
  };

  const containerStyle = {
    fontFamily: 'ui-monospace, SFMono-Regular, Menlo, Monaco, monospace',
    padding: '12px',
    background: 'var(--vscode-editor-background, #1e1e1e)',
    borderRadius: '8px',
    border: '1px solid var(--vscode-panel-border, #3c3c3c)'
  };

  const controlsStyle = {
    display: 'flex',
    flexDirection: 'column',
    gap: '8px',
    marginTop: '12px',
    padding: '10px',
    background: 'rgba(255,255,255,0.03)',
    borderRadius: '6px'
  };

  const rowStyle = {
    display: 'flex',
    alignItems: 'center',
    gap: '10px'
  };

  const labelStyle = {
    fontSize: '11px',
    color: 'var(--vscode-descriptionForeground, #858585)',
    minWidth: '70px'
  };

  const valueStyle = {
    fontSize: '11px',
    color: 'var(--vscode-foreground, #cccccc)',
    minWidth: '50px',
    fontWeight: '600'
  };

  const sliderStyle = {
    flex: 1,
    height: '4px',
    cursor: 'pointer',
    accentColor: props.color
  };

  return e('div', { style: containerStyle },
    e(ResponsiveContainer, { width: props.width, height: props.height },
      e(LineChart, { data: props.data },
        e(CartesianGrid, { strokeDasharray: '3 3', stroke: '#333' }),
        e(XAxis, { dataKey: 'x', stroke: '#666', tick: { fill: '#888', fontSize: 10 } }),
        e(YAxis, { stroke: '#666', tick: { fill: '#888', fontSize: 10 } }),
        e(Tooltip, {
          contentStyle: { background: '#2d2d2d', border: '1px solid #444', borderRadius: 4 },
          labelStyle: { color: '#aaa' }
        }),
        e(Line, { type: 'monotone', dataKey: 'y', stroke: props.color, dot: false, strokeWidth: 2 })
      )
    ),
    e('div', { style: controlsStyle },
      e('div', { style: rowStyle },
        e('span', { style: labelStyle }, 'Domain Lo'),
        e('input', {
          type: 'range',
          min: -10,
          max: domainHi - 0.1,
          step: 0.1,
          value: domainLo,
          onChange: handleDomainLoChange,
          style: sliderStyle
        }),
        e('span', { style: valueStyle }, domainLo.toFixed(2))
      ),
      e('div', { style: rowStyle },
        e('span', { style: labelStyle }, 'Domain Hi'),
        e('input', {
          type: 'range',
          min: domainLo + 0.1,
          max: 10,
          step: 0.1,
          value: domainHi,
          onChange: handleDomainHiChange,
          style: sliderStyle
        }),
        e('span', { style: valueStyle }, domainHi.toFixed(2))
      ),
      e('div', { style: rowStyle },
        e('span', { style: labelStyle }, 'Samples'),
        e('input', {
          type: 'range',
          min: 10,
          max: 500,
          step: 10,
          value: samples,
          onChange: handleSamplesChange,
          style: sliderStyle
        }),
        e('span', { style: valueStyle }, samples)
      )
    )
  );
}
"

/-! ## Helper Functions -/

/-- Sample a function and return JSON data -/
def sampleFunction (f : Float → Float) (lo hi : Float) (n : Nat) : Json :=
  if n == 0 then Json.arr #[] else
    let arr := (List.range (n + 1)).toArray.map fun i =>
      let x := lo + (hi - lo) * i.toFloat / n.toFloat
      let y := f x
      Json.mkObj [("x", toJson x), ("y", toJson y)]
    Json.arr arr

end LeanPlot.Interactive

/-! ## Command Syntax -/

open LeanPlot.Interactive

/-- Interactive plot command with sliders for domain and samples -/
syntax (name := iplotCmd) "#iplot " term ("domain=" "(" num "," num ")")? ("samples=" num)? : command

/-- Command elaborator for #iplot -/
@[command_elab iplotCmd]
def elabIplotCmd : CommandElab
  | stx@`(#iplot $fn:term $[domain=($lo:num, $hi:num)]? $[samples=$n:num]?) => do
    -- Parse parameters with defaults
    let domainLo : Float := lo.map (fun l => l.getNat.toFloat) |>.getD (-1.0)
    let domainHi : Float := hi.map (fun h => h.getNat.toFloat) |>.getD 1.0
    let samples : Nat := n.map (·.getNat) |>.getD 200

    -- Get document info for edits
    let fileMap ← getFileMap
    let some range := fileMap.lspRangeOfStx? stx
      | throwError "Could not determine source range"
    let lineNum := range.start.line
    let fileName ← getFileName
    let uri := s!"file://{fileName}"

    -- For now, use a simple quadratic function for demo
    -- In a full implementation, we'd interpret the term
    let f : Float → Float := fun x => x * x
    let data := sampleFunction f domainLo domainHi samples

    -- Build props
    let props : InteractivePlotProps := {
      data := data
      domainLo := domainLo
      domainHi := domainHi
      samples := samples
      lineNum := lineNum
      uri := uri
      width := 400
      height := 250
      color := "#4fc3f7"
    }

    -- Show widget
    let msg ← liftCoreM <| MessageData.ofComponent InteractivePlotPanel props
      s!"[Interactive Plot: domain=({domainLo}, {domainHi}) samples={samples}]"
    logInfo msg

  | _ => throwError "Unexpected #iplot syntax"

/-! ## Integration with Graphic Type -/

namespace LeanPlot

/-- Render a Graphic with interactive slider controls.
This wraps the standard render with additional sliders for:
- Domain (lo, hi)
- Sample count
- (Future: colors, titles, etc.)
-/
def Graphic.renderInteractive (g : Graphic) (lineNum : Nat) (uri : String) : Html :=
  -- For now, just use the standard render
  -- Future: wrap with slider widget component
  render g

end LeanPlot
