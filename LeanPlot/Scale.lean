import LeanPlot.ToFloat

/-! # LeanPlot.Scale

This module provides support for different scale types in charts, including
linear and logarithmic scales. This enables more flexible visualization of
data with different characteristics.
-/

namespace LeanPlot.Scale

/-- Scale type for chart axes -/
inductive ScaleType where
  /-- Linear scale: values are displayed proportionally -/
  | Linear
  /-- Logarithmic scale: values are displayed on a log scale with the given base -/
  | Logarithmic (base : Float := 10.0)
  deriving Repr

/-- Apply a scale transformation to a value -/
@[inline] def transform (scale : ScaleType) (value : Float) : Float :=
  match scale with
  | ScaleType.Linear => value
  | ScaleType.Logarithmic base =>
    if value > 0 then Float.log value / Float.log base
    else 0  -- Handle non-positive values by mapping to 0

/-- Inverse transform from scaled space back to original space -/
@[inline] def inverseTransform (scale : ScaleType) (value : Float) : Float :=
  match scale with
  | ScaleType.Linear => value
  | ScaleType.Logarithmic base => base ^ value

/-- Transform an array of values according to the given scale -/
@[inline] def transformArray (scale : ScaleType) (values : Array Float) : Array Float :=
  values.map (transform scale)

/-- Configuration for scale settings on a chart -/
structure ScaleConfig where
  /-- Scale type for the x-axis -/
  xScale : ScaleType := ScaleType.Linear
  /-- Scale type for the y-axis -/
  yScale : ScaleType := ScaleType.Linear

end LeanPlot.Scale
