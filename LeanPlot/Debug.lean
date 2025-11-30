import ProofWidgets.Component.HtmlDisplay

open Lean ProofWidgets
open scoped ProofWidgets.Jsx

/-! # LeanPlot.Debug – debugging utilities

This namespace is **not** part of the stable user API.  It contains helpers
that aid library authors when visually inspecting charts.  Main entry point
is `withSavePNG` which renders a *Save PNG* button above any chart. -/

namespace LeanPlot.Debug

/-- Props for the `SavePNG` React component used by `withSavePNG`.
Each instance identifies a DOM element to rasterise and the filename for
the resulting download. -/
structure SavePNGProps where
  /-- `HTMLElement` id whose visual contents will be rasterised. -/
  targetId : String
  /-- Desired filename for the downloaded PNG. -/
  fileName : String := "chart.png"
  deriving FromJson, ToJson

/-- Tiny React helper that uses `html2canvas` to download a PNG snapshot of
the element identified by `targetId`.  This component intentionally keeps a
minimal API and is considered part of the debug surface (not the stable
public API). -/
@[widget_module] def SavePNG : ProofWidgets.Component SavePNGProps where
  javascript := "import html2canvas from 'https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/+esm';\n\nexport function SavePNG(props) {\n  const handleClick = () => {\n    const el = document.getElementById(props.targetId);\n    if (!el) { console.error('SavePNG: target not found'); return; }\n    html2canvas(el).then(canvas => {\n      const link = document.createElement('a');\n      link.download = props.fileName;\n      link.href = canvas.toDataURL();\n      link.click();\n    });\n  };\n  return React.createElement('button', { onClick: handleClick, style: { marginBottom: '4px' } }, 'Save PNG');\n}\n"
  «export» := "SavePNG"

/-- Wrap a `Html` plot with a one‑click “Save PNG” button.
The `id` must be unique if multiple wrapped charts appear on the same page. -/
@[inline] def withSavePNG (plot : Html) (id : String := "leanplot-debug-target") (fileName := "chart.png") : Html :=
  <div>
    <SavePNG targetId={id} fileName={fileName}/>
    <div id={id}>{plot}</div>
  </div>

end LeanPlot.Debug
