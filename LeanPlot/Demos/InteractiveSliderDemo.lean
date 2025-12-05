import LeanPlot.Interactive

/-!
# Interactive Slider Demo

This demo showcases the 2-way slider widgets for interactive plot parameters.

Each parameter section has sliders that write back to source code when adjusted.
If a parameter is at its default value, clicking "+ Add to source" explicitly
adds it to the command.

## Usage

Hover over the widget output to see the interactive panel with sliders for:
- **Domain** (min, max) - x-axis range
- **Steps** - number of sample points
- **Size** (width, height) - chart dimensions

Dragging any slider automatically updates the source code!
-/

open LeanPlot.Interactive

-- Basic: Just a function, all defaults
-- Try hovering and adjusting sliders - they'll write back to source
#iplot (fun x => x * x)

-- With explicit domain
-- Notice the "explicit" badge on Domain - it's in the source
#iplot (fun x => x * x) domain=(-2, 2)

-- With explicit domain and steps
#iplot (fun x => x * x) domain=(-3, 3) steps=100

-- Fully explicit: all parameters
-- All sections show "explicit" badge
#iplot (fun x => x * x) domain=(-1, 1) steps=200 size=(500, 300)

-- Try with a sine-like pattern (represented as x^3 - x for demo)
#iplot (fun x => x^3 - x) domain=(-2, 2) steps=150

-- With explicit color
#iplot (fun x => x * x) domain=(-1, 1) color="#ff7043"

/-!
## How It Works

1. **Parameter Tracking**: The widget tracks which parameters are explicitly
   set in source vs using defaults.

2. **2-Way Binding**: When you drag a slider:
   - The widget state updates immediately for responsive feedback
   - The source code is updated via `applyEdit` API
   - The file re-elaborates, producing fresh data

3. **Progressive Disclosure**: Parameters start with defaults. Clicking
   "+ Add to source" makes them explicit, giving you fine control.

This pattern is inspired by the ImageViewer widget in Aloklib, which uses
the same technique for image resize sliders.
-/
