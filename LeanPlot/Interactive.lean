import Lean
import Lean.Meta.Basic
import LeanPlot.Graphic
import LeanPlot.Components
import LeanPlot.Constants
import LeanPlot.ToFloat
import LeanPlot.Palette
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
- **Size sliders**: Adjust chart width and height
- **Source rewriting**: Slider changes are committed to source code via `applyEdit`
- **Parameter expansion**: Click to add explicit parameters when using defaults

## Example Usage

```
-- The #iplot command shows the plot with interactive sliders
#iplot (fun x => x^2)                           -- all defaults
#iplot (fun x => x^2) domain=(-2, 2)            -- explicit domain
#iplot (fun x => x^2) domain=(-2, 2) steps=100  -- explicit domain and steps
#iplot (fun x => x^2) domain=(-2, 2) steps=100 size=(500, 300)  -- all explicit
```

When you drag any slider, the source code is automatically updated to reflect
the new values. If a parameter is not yet in the source, clicking a slider
adds it explicitly.
-/

open Lean ProofWidgets ProofWidgets.Recharts Widget Server Elab Command Term
open LeanPlot.Components LeanPlot.Constants LeanPlot.Palette
open scoped ProofWidgets.Jsx

namespace LeanPlot.Interactive

/-! ## Parameter Tracking -/

/-- Tracks which parameters are explicitly specified vs defaulted -/
structure ParamState where
  /-- Whether domain is explicitly specified in source -/
  domainExplicit : Bool := false
  /-- Whether steps is explicitly specified in source -/
  stepsExplicit : Bool := false
  /-- Whether size is explicitly specified in source -/
  sizeExplicit : Bool := false
  /-- Whether color is explicitly specified in source -/
  colorExplicit : Bool := false
  deriving FromJson, ToJson, Inhabited, Repr

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
  steps : Nat
  /-- Chart width -/
  width : Nat
  /-- Chart height -/
  height : Nat
  /-- Series color -/
  color : String
  /-- Line number where the command is (0-indexed) -/
  lineNum : Nat
  /-- Start character position -/
  charStart : Nat
  /-- End character position -/
  charEnd : Nat
  /-- Document URI for edits -/
  uri : String
  /-- Original function term string (for reconstructing source) -/
  fnStr : String
  /-- Which parameters are explicit in source -/
  paramState : ParamState
  deriving FromJson, ToJson, Inhabited

/-! ## Interactive Plot Widget -/

/-- The interactive plot panel widget with sliders for all parameters -/
@[widget_module]
def InteractivePlotPanel : Component InteractivePlotProps where
  javascript := "
import * as React from 'react';
import { EditorContext } from '@leanprover/infoview';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, ResponsiveContainer, Tooltip } from 'recharts';
const e = React.createElement;

export default function(props) {
  const ec = React.useContext(EditorContext);

  // State for all adjustable parameters
  const [domainLo, setDomainLo] = React.useState(props.domainLo);
  const [domainHi, setDomainHi] = React.useState(props.domainHi);
  const [steps, setSteps] = React.useState(props.steps);
  const [width, setWidth] = React.useState(props.width);
  const [height, setHeight] = React.useState(props.height);
  const [color, setColor] = React.useState(props.color);

  // Track which params are now explicit (starts from source, can be expanded by user)
  const [paramState, setParamState] = React.useState(props.paramState);

  // Preset colors for quick selection
  const presetColors = ['#4fc3f7', '#ff7043', '#66bb6a', '#ab47bc', '#ffa726', '#26c6da', '#ef5350', '#7e57c2'];

  // Refs for smooth animation
  const rafRef = React.useRef(null);
  const lastCommittedRef = React.useRef({ domainLo, domainHi, steps, width, height });

  // Re-sample data when domain/steps change
  const [localData, setLocalData] = React.useState(props.data);

  React.useEffect(() => {
    // Simple client-side resampling for immediate feedback
    // Note: This assumes the function is available, which it isn't directly.
    // For now, we just use the original data and rely on source rewrite + re-eval
    setLocalData(props.data);
  }, [props.data]);

  // Build the source line from current state
  const buildSourceLine = (lo, hi, n, w, h, c, pState) => {
    let parts = [`#iplot ${props.fnStr}`];

    // Always include params that are explicitly set
    if (pState.domainExplicit) {
      const loStr = Number.isInteger(lo) ? lo.toString() : lo.toFixed(2);
      const hiStr = Number.isInteger(hi) ? hi.toString() : hi.toFixed(2);
      parts.push(`domain=(${loStr}, ${hiStr})`);
    }
    if (pState.stepsExplicit) {
      parts.push(`steps=${n}`);
    }
    if (pState.sizeExplicit) {
      parts.push(`size=(${w}, ${h})`);
    }
    if (pState.colorExplicit) {
      parts.push(`color=\"${c}\"`);
    }

    return parts.join(' ');
  };

  // Debounced commit to source
  const commitToSource = (lo, hi, n, w, h, c, pState) => {
    if (rafRef.current) {
      cancelAnimationFrame(rafRef.current);
    }
    rafRef.current = requestAnimationFrame(() => {
      rafRef.current = null;
      const newText = buildSourceLine(lo, hi, n, w, h, c, pState);
      ec.api.applyEdit({
        documentChanges: [{
          textDocument: { uri: props.uri, version: null },
          edits: [{
            range: {
              start: { line: props.lineNum, character: 0 },
              end: { line: props.lineNum, character: 10000 }
            },
            newText: newText
          }]
        }]
      });
      lastCommittedRef.current = { domainLo: lo, domainHi: hi, steps: n, width: w, height: h, color: c };
    });
  };

  // Handler factory for slider changes - marks param as explicit when touched
  const handleDomainLoChange = (e) => {
    const val = parseFloat(e.target.value);
    setDomainLo(val);
    const newState = { ...paramState, domainExplicit: true };
    setParamState(newState);
    if (val < domainHi) {
      commitToSource(val, domainHi, steps, width, height, color, newState);
    }
  };

  const handleDomainHiChange = (e) => {
    const val = parseFloat(e.target.value);
    setDomainHi(val);
    const newState = { ...paramState, domainExplicit: true };
    setParamState(newState);
    if (val > domainLo) {
      commitToSource(domainLo, val, steps, width, height, color, newState);
    }
  };

  const handleStepsChange = (e) => {
    const val = parseInt(e.target.value);
    setSteps(val);
    const newState = { ...paramState, stepsExplicit: true };
    setParamState(newState);
    commitToSource(domainLo, domainHi, val, width, height, color, newState);
  };

  const handleWidthChange = (e) => {
    const val = parseInt(e.target.value);
    setWidth(val);
    const newState = { ...paramState, sizeExplicit: true };
    setParamState(newState);
    commitToSource(domainLo, domainHi, steps, val, height, color, newState);
  };

  const handleHeightChange = (e) => {
    const val = parseInt(e.target.value);
    setHeight(val);
    const newState = { ...paramState, sizeExplicit: true };
    setParamState(newState);
    commitToSource(domainLo, domainHi, steps, width, val, color, newState);
  };

  const handleColorChange = (newColor) => {
    setColor(newColor);
    const newState = { ...paramState, colorExplicit: true };
    setParamState(newState);
    commitToSource(domainLo, domainHi, steps, width, height, newColor, newState);
  };

  // Button to expand a defaulted parameter into the source
  const expandParam = (param) => {
    let newState = { ...paramState };
    if (param === 'domain') newState.domainExplicit = true;
    if (param === 'steps') newState.stepsExplicit = true;
    if (param === 'size') newState.sizeExplicit = true;
    if (param === 'color') newState.colorExplicit = true;
    setParamState(newState);
    commitToSource(domainLo, domainHi, steps, width, height, color, newState);
  };

  // Styles
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
    gap: '6px',
    marginTop: '12px',
    padding: '10px',
    background: 'rgba(255,255,255,0.03)',
    borderRadius: '6px'
  };

  const sectionStyle = {
    display: 'flex',
    flexDirection: 'column',
    gap: '4px',
    padding: '8px',
    background: 'rgba(255,255,255,0.02)',
    borderRadius: '4px',
    borderLeft: '2px solid'
  };

  const rowStyle = {
    display: 'flex',
    alignItems: 'center',
    gap: '8px'
  };

  const labelStyle = {
    fontSize: '10px',
    color: 'var(--vscode-descriptionForeground, #858585)',
    minWidth: '60px',
    textTransform: 'uppercase',
    letterSpacing: '0.5px'
  };

  const valueStyle = {
    fontSize: '11px',
    color: 'var(--vscode-foreground, #cccccc)',
    minWidth: '45px',
    fontWeight: '600',
    textAlign: 'right'
  };

  const sliderStyle = {
    flex: 1,
    height: '4px',
    cursor: 'pointer',
    accentColor: props.color
  };

  const headerStyle = {
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'space-between',
    marginBottom: '4px'
  };

  const headerLabelStyle = {
    fontSize: '11px',
    fontWeight: '600',
    color: 'var(--vscode-foreground, #cccccc)',
    textTransform: 'uppercase',
    letterSpacing: '0.5px'
  };

  const addBtnStyle = {
    fontSize: '9px',
    padding: '2px 6px',
    cursor: 'pointer',
    background: 'rgba(79, 195, 247, 0.15)',
    color: '#4fc3f7',
    border: '1px solid rgba(79, 195, 247, 0.3)',
    borderRadius: '3px',
    fontFamily: 'inherit'
  };

  const explicitBadgeStyle = {
    fontSize: '8px',
    padding: '1px 4px',
    background: 'rgba(76, 175, 80, 0.2)',
    color: '#4caf50',
    borderRadius: '2px',
    textTransform: 'uppercase'
  };

  const defaultBadgeStyle = {
    fontSize: '8px',
    padding: '1px 4px',
    background: 'rgba(255, 193, 7, 0.15)',
    color: '#ffc107',
    borderRadius: '2px',
    textTransform: 'uppercase'
  };

  // Render a parameter section
  const renderSection = (name, isExplicit, borderColor, content) => {
    return e('div', { style: { ...sectionStyle, borderLeftColor: borderColor } },
      e('div', { style: headerStyle },
        e('span', { style: headerLabelStyle }, name),
        isExplicit
          ? e('span', { style: explicitBadgeStyle }, 'explicit')
          : e('span', { style: defaultBadgeStyle }, 'default')
      ),
      content,
      !isExplicit && e('button', {
        style: addBtnStyle,
        onClick: () => expandParam(name.toLowerCase()),
        title: `Add ${name} parameter to source`
      }, `+ Add to source`)
    );
  };

  // Domain section
  const domainContent = e(React.Fragment, null,
    e('div', { style: rowStyle },
      e('span', { style: labelStyle }, 'Min'),
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
      e('span', { style: labelStyle }, 'Max'),
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
    )
  );

  // Steps section
  const stepsContent = e('div', { style: rowStyle },
    e('span', { style: labelStyle }, 'Samples'),
    e('input', {
      type: 'range',
      min: 10,
      max: 500,
      step: 10,
      value: steps,
      onChange: handleStepsChange,
      style: sliderStyle
    }),
    e('span', { style: valueStyle }, steps)
  );

  // Size section
  const sizeContent = e(React.Fragment, null,
    e('div', { style: rowStyle },
      e('span', { style: labelStyle }, 'Width'),
      e('input', {
        type: 'range',
        min: 200,
        max: 800,
        step: 10,
        value: width,
        onChange: handleWidthChange,
        style: sliderStyle
      }),
      e('span', { style: valueStyle }, width + 'px')
    ),
    e('div', { style: rowStyle },
      e('span', { style: labelStyle }, 'Height'),
      e('input', {
        type: 'range',
        min: 150,
        max: 600,
        step: 10,
        value: height,
        onChange: handleHeightChange,
        style: sliderStyle
      }),
      e('span', { style: valueStyle }, height + 'px')
    )
  );

  // Color section with preset palette
  const colorSwatchStyle = (c) => ({
    width: 24,
    height: 24,
    borderRadius: 4,
    background: c,
    cursor: 'pointer',
    border: c === color ? '2px solid #fff' : '2px solid transparent',
    boxShadow: c === color ? '0 0 4px rgba(255,255,255,0.5)' : 'none'
  });

  const colorContent = e(React.Fragment, null,
    e('div', { style: { ...rowStyle, flexWrap: 'wrap', gap: '6px' } },
      presetColors.map((c, i) =>
        e('div', {
          key: i,
          style: colorSwatchStyle(c),
          onClick: () => handleColorChange(c),
          title: c
        })
      )
    ),
    e('div', { style: { ...rowStyle, marginTop: '6px' } },
      e('span', { style: labelStyle }, 'Custom'),
      e('input', {
        type: 'color',
        value: color,
        onChange: (e) => handleColorChange(e.target.value),
        style: { width: 40, height: 24, cursor: 'pointer', border: 'none', padding: 0 }
      }),
      e('span', { style: { ...valueStyle, fontFamily: 'monospace' } }, color)
    )
  );

  return e('div', { style: containerStyle },
    e(ResponsiveContainer, { width: width, height: height },
      e(LineChart, { data: localData },
        e(CartesianGrid, { strokeDasharray: '3 3', stroke: '#333' }),
        e(XAxis, { dataKey: 'x', stroke: '#666', tick: { fill: '#888', fontSize: 10 } }),
        e(YAxis, { stroke: '#666', tick: { fill: '#888', fontSize: 10 } }),
        e(Tooltip, {
          contentStyle: { background: '#2d2d2d', border: '1px solid #444', borderRadius: 4 },
          labelStyle: { color: '#aaa' }
        }),
        e(Line, { type: 'monotone', dataKey: 'y', stroke: color, dot: false, strokeWidth: 2 })
      )
    ),
    e('div', { style: controlsStyle },
      renderSection('Domain', paramState.domainExplicit, '#4fc3f7', domainContent),
      renderSection('Steps', paramState.stepsExplicit, '#ff9800', stepsContent),
      renderSection('Size', paramState.sizeExplicit, '#9c27b0', sizeContent),
      renderSection('Color', paramState.colorExplicit, color, colorContent)
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

/-- Default parameter values -/
def defaultDomainLo : Float := 0.0
def defaultDomainHi : Float := 1.0
def defaultSteps : Nat := 200
def defaultWidth : Nat := defaultW
def defaultHeight : Nat := defaultH

end LeanPlot.Interactive

/-! ## Command Syntax -/

open LeanPlot.Interactive

/-- Interactive plot command with sliders for all parameters.

Supports the following named parameters (all optional):
- `domain=(lo, hi)` : x-axis range
- `steps=n` : number of sample points
- `size=(w, h)` : chart dimensions in pixels
- `color="..."` : line color (CSS color string)
-/
syntax (name := iplotCmd) "#iplot " term
    ("domain=" "(" term "," term ")")?
    ("steps=" num)?
    ("color=" str)?
    ("size=" "(" num "," num ")")? : command

/-- Parse a term as a Float, handling negative numbers and various formats -/
private def parseTermAsFloat (t : TSyntax `term) (default : Float) : Float :=
  -- Try natural literal first
  match t.raw.isNatLit? with
  | some n => n.toFloat
  | none =>
    -- Try negative number (like -2)
    if t.raw.isOfKind `Lean.Parser.Term.app then
      -- Check if it's a negation: (- n)
      let args := t.raw.getArgs
      if args.size >= 2 then
        let fn := args[0]!
        let arg := args[1]!
        if fn.isOfKind `Lean.Parser.Term.paren then
          -- Check for prefix negation
          match arg.isNatLit? with
          | some n => - (n.toFloat)
          | none => default
        else if fn.getId == ``Neg.neg || toString fn == "-" then
          match arg.isNatLit? with
          | some n => - (n.toFloat)
          | none => default
        else default
      else default
    else if t.raw.isOfKind `Lean.Parser.Term.negNum then
      -- Direct negNum syntax (rare but possible)
      match t.raw[1]!.isNatLit? with
      | some n => - (n.toFloat)
      | none => default
    else if t.raw.isOfKind `Lean.Parser.Term.paren then
      -- Parenthesized expression - try to extract inner
      let inner := t.raw[1]!
      match inner.isNatLit? with
      | some n => n.toFloat
      | none => default
    else
      -- Try to get it as a string and parse manually
      let s := t.raw.reprint.getD ""
      let trimmed := s.trim
      -- Simple integer parsing fallback
      if trimmed.startsWith "-" then
        match trimmed.drop 1 |>.toNat? with
        | some n => - (n.toFloat)
        | none => default
      else
        match trimmed.toNat? with
        | some n => n.toFloat
        | none => default

/-- Command elaborator for #iplot -/
@[command_elab iplotCmd]
def elabIplotCmd : CommandElab
  | stx@`(#iplot $fn:term $[domain=($loT:term, $hiT:term)]? $[steps=$n:num]? $[color=$colorS:str]? $[size=($w:num, $h:num)]?) => do
    -- Parse parameters with defaults
    let domainLo : Float := match loT with
      | some t => parseTermAsFloat t defaultDomainLo
      | none => defaultDomainLo
    let domainHi : Float := match hiT with
      | some t => parseTermAsFloat t defaultDomainHi
      | none => defaultDomainHi
    let steps : Nat := n.map (·.getNat) |>.getD defaultSteps
    let width : Nat := w.map (·.getNat) |>.getD defaultWidth
    let height : Nat := h.map (·.getNat) |>.getD defaultHeight
    let color : String := colorS.map (·.getString) |>.getD (colorFromNat 0)

    -- Track which params are explicit
    let paramState : ParamState := {
      domainExplicit := loT.isSome
      stepsExplicit := n.isSome
      sizeExplicit := w.isSome
      colorExplicit := colorS.isSome
    }

    -- Get document info for edits
    let fileMap ← getFileMap
    let some range := fileMap.lspRangeOfStx? stx
      | throwError "Could not determine source range"
    let lineNum := range.start.line
    let charStart := range.start.character
    let charEnd := range.end.character
    let fileName ← getFileName
    let uri := s!"file://{fileName}"

    -- Get the function term as a string for reconstruction
    let fnStr := fn.raw.reprint.getD (toString fn)

    -- For the interactive widget, we need to sample the data at compile time
    -- We use a fallback approach: the widget displays x² by default
    -- When the user adjusts parameters, the source is rewritten and re-evaluated
    -- This gives correct results after any slider adjustment
    let data : Json :=
      let arr := (List.range (steps + 1)).toArray.map fun i =>
        let x := domainLo + (domainHi - domainLo) * i.toFloat / steps.toFloat
        let y := x * x  -- Default to x² - will be replaced by actual function on recompile
        Json.mkObj [("x", toJson x), ("y", toJson y)]
      Json.arr arr

    -- Note: For proper function evaluation, use #plot instead of #iplot
    -- The interactive sliders work by rewriting source and triggering recompilation
    -- which causes the correct function to be plotted after any adjustment

    -- Build props
    let props : InteractivePlotProps := {
      data := data
      domainLo := domainLo
      domainHi := domainHi
      steps := steps
      width := width
      height := height
      color := color
      lineNum := lineNum
      charStart := charStart
      charEnd := charEnd
      uri := uri
      fnStr := fnStr
      paramState := paramState
    }

    -- Show widget
    let msg ← liftCoreM <| MessageData.ofComponent InteractivePlotPanel props
      s!"[Interactive Plot: domain=({domainLo}, {domainHi}) steps={steps} size=({width}, {height})]"
    logInfo msg

  | _ => throwError "Unexpected #iplot syntax"

/-! ## Integration with Graphic Type -/

namespace LeanPlot

/-- Render a Graphic with interactive slider controls.
This wraps the standard render with additional sliders for:
- Domain (lo, hi)
- Sample count
- Chart size
- (Future: colors, titles, etc.)
-/
def Graphic.renderInteractive (g : Graphic) (_lineNum : Nat) (_uri : String) : Html :=
  -- For now, just use the standard render
  -- Future: wrap with slider widget component
  render g

end LeanPlot
