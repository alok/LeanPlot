import Lean
import LeanPlot.Specification
import LeanPlot.Series
import ProofWidgets.Component.Panel.Basic
import ProofWidgets.Component.Recharts

open Lean ProofWidgets ProofWidgets.Recharts
open scoped ProofWidgets.Jsx

namespace LeanPlot.TunablePlot

/-! ## Tunable plot props -/

structure TunableSeries where
  name : String
  dataKey : String
  kind : String
  color : String
  dot : Bool := false
  deriving FromJson, ToJson, Inhabited

structure TunablePlotProps where
  data : Json
  series : Array TunableSeries
  width : Nat
  height : Nat
  title : String
  xLabel : String
  yLabel : String
  xDomain : Option (Array Float)
  yDomain : Option (Array Float)
  legend : Bool
  lineNum : Nat
  uri : String
  termStr : String
  caption : String
  deriving FromJson, ToJson, Inhabited

/-! ## Tunable plot widget -/

@[widget_module]
def TunablePlotPanel : Component TunablePlotProps where
  javascript := "
import * as React from 'react';
import { EditorContext } from '@leanprover/infoview';
import { LineChart, Line, XAxis, YAxis, Tooltip, Legend, AreaChart, BarChart, ComposedChart, ScatterChart, Scatter, Area, Bar } from 'recharts';
const e = React.createElement;

const fmtNum = (x) => Number.isInteger(x) ? x.toString() : x.toFixed(2);
const escapeString = (s) => s.replace(/\\\\/g, '\\\\\\\\').replace(/\"/g, '\\\\\"');

const computeExtent = (data, key) => {
  let min = Infinity;
  let max = -Infinity;
  for (const row of data) {
    const v = row?.[key];
    if (typeof v === 'number' && Number.isFinite(v)) {
      if (v < min) min = v;
      if (v > max) max = v;
    }
  }
  if (min === Infinity || max === -Infinity) return { min: 0, max: 1 };
  if (min === max) return { min: min - 1, max: max + 1 };
  return { min, max };
};

const computeYExtent = (data, series) => {
  let min = Infinity;
  let max = -Infinity;
  for (const s of series) {
    const ext = computeExtent(data, s.dataKey);
    if (ext.min < min) min = ext.min;
    if (ext.max > max) max = ext.max;
  }
  if (min === Infinity || max === -Infinity) return { min: 0, max: 1 };
  if (min === max) return { min: min - 1, max: max + 1 };
  return { min, max };
};

export default function(props) {
  const ec = React.useContext(EditorContext);
  const data = Array.isArray(props.data) ? props.data : [];
  const series = Array.isArray(props.series) ? props.series : [];

  const xExt = computeExtent(data, 'x');
  const yExt = computeYExtent(data, series);
  const xPad = (xExt.max - xExt.min) * 0.1;
  const yPad = (yExt.max - yExt.min) * 0.1;
  const xMinBound = xExt.min - xPad;
  const xMaxBound = xExt.max + xPad;
  const yMinBound = yExt.min - yPad;
  const yMaxBound = yExt.max + yPad;

  const [width, setWidth] = React.useState(props.width);
  const [height, setHeight] = React.useState(props.height);
  const [legend, setLegend] = React.useState(props.legend);
  const [title, setTitle] = React.useState(props.title || '');
  const [xLabel, setXLabel] = React.useState(props.xLabel || '');
  const [yLabel, setYLabel] = React.useState(props.yLabel || '');
  const [xMin, setXMin] = React.useState(props.xDomain ? props.xDomain[0] : xExt.min);
  const [xMax, setXMax] = React.useState(props.xDomain ? props.xDomain[1] : xExt.max);
  const [yMin, setYMin] = React.useState(props.yDomain ? props.yDomain[0] : yExt.min);
  const [yMax, setYMax] = React.useState(props.yDomain ? props.yDomain[1] : yExt.max);
  const [colors, setColors] = React.useState(series.map(s => s.color));

  const rafRef = React.useRef(null);

  const buildSourceLine = () => {
    const base = `(LeanPlot.toPlotSpec ${props.termStr})`;
    const parts = [];
    parts.push(`|> LeanPlot.PlotSpec.withSize ${width} ${height}`);
    parts.push(`|> LeanPlot.PlotSpec.withLegend ${legend ? 'true' : 'false'}`);
    if (Number.isFinite(xMin) && Number.isFinite(xMax)) {
      parts.push(`|> LeanPlot.PlotSpec.withXDomain ${fmtNum(xMin)} ${fmtNum(xMax)}`);
    }
    if (Number.isFinite(yMin) && Number.isFinite(yMax)) {
      parts.push(`|> LeanPlot.PlotSpec.withYDomain ${fmtNum(yMin)} ${fmtNum(yMax)}`);
    }
    if (title.trim().length > 0) {
      parts.push(`|> LeanPlot.PlotSpec.withTitle \\\"${escapeString(title)}\\\"`);
    }
    if (xLabel.trim().length > 0) {
      parts.push(`|> LeanPlot.PlotSpec.withXLabel \\\"${escapeString(xLabel)}\\\"`);
    }
    if (yLabel.trim().length > 0) {
      parts.push(`|> LeanPlot.PlotSpec.withYLabel \\\"${escapeString(yLabel)}\\\"`);
    }
    for (let i = 0; i < colors.length; i++) {
      parts.push(`|> LeanPlot.PlotSpec.withSeriesColorAt ${i} \\\"${escapeString(colors[i])}\\\"`);
    }
    return `#plot +tunable ${base} ${parts.join(' ')}`;
  };

  const commitToSource = () => {
    if (rafRef.current) cancelAnimationFrame(rafRef.current);
    rafRef.current = requestAnimationFrame(() => {
      rafRef.current = null;
      const newText = buildSourceLine();
      ec.api.applyEdit({
        documentChanges: [{
          textDocument: { uri: props.uri, version: null },
          edits: [{
            range: {
              start: { line: props.lineNum, character: 0 },
              end: { line: props.lineNum, character: 10000 }
            },
            newText
          }]
        }]
      });
    });
  };

  const updateColor = (idx, newColor) => {
    const next = colors.slice();
    next[idx] = newColor;
    setColors(next);
    commitToSource();
  };

  const setDomain = (which, val) => {
    if (which === 'xMin') setXMin(val);
    if (which === 'xMax') setXMax(val);
    if (which === 'yMin') setYMin(val);
    if (which === 'yMax') setYMax(val);
    commitToSource();
  };

  const kinds = Array.from(new Set(series.map(s => s.kind)));
  const isMixed = kinds.length > 1;
  const allAre = (k) => kinds.length === 1 && kinds[0] === k;

  const renderSeries = () => series.map((s, i) => {
    if (s.kind === 'line') {
      return e(Line, { key: i, type: 'monotone', dataKey: s.dataKey, stroke: colors[i], dot: s.dot });
    }
    if (s.kind === 'scatter') {
      return e(Scatter, { key: i, dataKey: s.dataKey, fill: colors[i] });
    }
    if (s.kind === 'bar') {
      return e(Bar, { key: i, dataKey: s.dataKey, fill: colors[i] });
    }
    if (s.kind === 'area') {
      return e(Area, { key: i, dataKey: s.dataKey, fill: colors[i], stroke: colors[i] });
    }
    return null;
  });

  const axisProps = {
    xAxis: e(XAxis, { dataKey: 'x', domain: [xMin, xMax], label: xLabel || undefined }),
    yAxis: e(YAxis, { domain: [yMin, yMax], label: yLabel ? { value: yLabel, angle: -90, position: 'left' } : undefined })
  };

  const chartProps = { width, height, data };
  let chart = null;
  if (isMixed) {
    chart = e(ComposedChart, chartProps, axisProps.xAxis, axisProps.yAxis, legend && e(Legend, {}), e(Tooltip, {}), ...renderSeries());
  } else if (allAre('bar')) {
    chart = e(BarChart, chartProps, axisProps.xAxis, axisProps.yAxis, legend && e(Legend, {}), e(Tooltip, {}), ...renderSeries());
  } else if (allAre('area')) {
    chart = e(AreaChart, chartProps, axisProps.xAxis, axisProps.yAxis, legend && e(Legend, {}), e(Tooltip, {}), ...renderSeries());
  } else if (allAre('scatter')) {
    chart = e(ScatterChart, chartProps, axisProps.xAxis, axisProps.yAxis, legend && e(Legend, {}), e(Tooltip, {}), ...renderSeries());
  } else {
    chart = e(LineChart, chartProps, axisProps.xAxis, axisProps.yAxis, legend && e(Legend, {}), e(Tooltip, {}), ...renderSeries());
  }

  const containerStyle = {
    fontFamily: 'ui-monospace, SFMono-Regular, Menlo, Monaco, monospace',
    padding: '12px',
    background: 'var(--vscode-editor-background, #1e1e1e)',
    borderRadius: '8px',
    border: '1px solid var(--vscode-panel-border, #3c3c3c)',
    display: 'flex',
    flexDirection: 'column',
    gap: '10px'
  };

  const controlsStyle = {
    display: 'grid',
    gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))',
    gap: '8px'
  };

  const sectionStyle = {
    padding: '8px',
    background: 'rgba(255,255,255,0.03)',
    borderRadius: '6px'
  };

  const labelStyle = { fontSize: '10px', color: 'var(--vscode-descriptionForeground, #858585)', textTransform: 'uppercase' };
  const inputStyle = { width: '100%' };
  const headerStyle = { fontSize: '11px', fontWeight: 600, marginBottom: '6px' };

  return e('div', { style: containerStyle },
    props.caption && e('div', { style: { fontSize: '12px', color: '#9aa0a6' } }, props.caption),
    title && e('div', { style: { fontSize: '12px', fontWeight: 600 } }, title),
    chart,
    e('div', { style: controlsStyle },
      e('div', { style: sectionStyle },
        e('div', { style: headerStyle }, 'Size'),
        e('label', { style: labelStyle }, `Width: ${width}px`),
        e('input', { type: 'range', min: 200, max: 1200, step: 10, value: width,
          onChange: (e) => { setWidth(parseInt(e.target.value)); commitToSource(); }, style: inputStyle }),
        e('label', { style: labelStyle }, `Height: ${height}px`),
        e('input', { type: 'range', min: 150, max: 900, step: 10, value: height,
          onChange: (e) => { setHeight(parseInt(e.target.value)); commitToSource(); }, style: inputStyle })
      ),
      e('div', { style: sectionStyle },
        e('div', { style: headerStyle }, 'Legend & Labels'),
        e('label', { style: labelStyle }, 'Legend'),
        e('input', { type: 'checkbox', checked: legend,
          onChange: (e) => { setLegend(e.target.checked); commitToSource(); } }),
        e('label', { style: labelStyle }, 'Title'),
        e('input', { type: 'text', value: title, onChange: (e) => { setTitle(e.target.value); commitToSource(); }, style: inputStyle }),
        e('label', { style: labelStyle }, 'X Label'),
        e('input', { type: 'text', value: xLabel, onChange: (e) => { setXLabel(e.target.value); commitToSource(); }, style: inputStyle }),
        e('label', { style: labelStyle }, 'Y Label'),
        e('input', { type: 'text', value: yLabel, onChange: (e) => { setYLabel(e.target.value); commitToSource(); }, style: inputStyle })
      ),
      e('div', { style: sectionStyle },
        e('div', { style: headerStyle }, 'X Domain'),
        e('label', { style: labelStyle }, `Min: ${fmtNum(xMin)}`),
        e('input', { type: 'range', min: xMinBound, max: xMax - 0.0001, step: (xMaxBound - xMinBound) / 200,
          value: xMin, onChange: (e) => setDomain('xMin', parseFloat(e.target.value)), style: inputStyle }),
        e('label', { style: labelStyle }, `Max: ${fmtNum(xMax)}`),
        e('input', { type: 'range', min: xMin + 0.0001, max: xMaxBound, step: (xMaxBound - xMinBound) / 200,
          value: xMax, onChange: (e) => setDomain('xMax', parseFloat(e.target.value)), style: inputStyle })
      ),
      e('div', { style: sectionStyle },
        e('div', { style: headerStyle }, 'Y Domain'),
        e('label', { style: labelStyle }, `Min: ${fmtNum(yMin)}`),
        e('input', { type: 'range', min: yMinBound, max: yMax - 0.0001, step: (yMaxBound - yMinBound) / 200,
          value: yMin, onChange: (e) => setDomain('yMin', parseFloat(e.target.value)), style: inputStyle }),
        e('label', { style: labelStyle }, `Max: ${fmtNum(yMax)}`),
        e('input', { type: 'range', min: yMin + 0.0001, max: yMaxBound, step: (yMaxBound - yMinBound) / 200,
          value: yMax, onChange: (e) => setDomain('yMax', parseFloat(e.target.value)), style: inputStyle })
      ),
      e('div', { style: sectionStyle },
        e('div', { style: headerStyle }, 'Series Colors'),
        series.map((s, i) =>
          e('div', { key: i, style: { display: 'flex', alignItems: 'center', gap: '8px', marginBottom: '6px' } },
            e('span', { style: { fontSize: '10px', width: '70px', color: '#bbb' } }, s.name),
            e('input', { type: 'color', value: colors[i], onChange: (e) => updateColor(i, e.target.value) }),
            e('span', { style: { fontSize: '10px', color: '#888' } }, colors[i])
          )
        )
      )
    )
  );
}
"

end LeanPlot.TunablePlot
