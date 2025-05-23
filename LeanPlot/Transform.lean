import LeanPlot.ToFloat
import LeanPlot.Constants

/-! # LeanPlot.Transform – Data transformation utilities

This module provides utilities for transforming data before plotting, including
logarithmic scales, normalizations, and custom transformations. -/

namespace LeanPlot.Transform

/-- Helper to get sign of a float -/
def _root_.Float.sign (x : Float) : Float :=
  if x > 0 then 1 else if x < 0 then -1 else 0

/-- A scale transformation for plot axes -/
structure Scale where
  /-- Transform from data space to plot space -/
  forward : Float → Float
  /-- Inverse transform from plot space to data space -/
  inverse : Float → Float
  /-- Human-readable name for the scale -/
  name : String
  deriving Inhabited

/-- Linear scale (identity transformation) -/
def linearScale : Scale := {
  forward := id,
  inverse := id,
  name := "linear"
}

/-- Logarithmic scale with specified base -/
def logScale (base : Float := 10.0) : Scale := {
  forward := fun x => if x > 0 then Float.log x / Float.log base else 0,
  inverse := fun y => base ^ y,
  name := s!"log{base}"
}

/-- Square root scale (useful for area comparisons) -/
def sqrtScale : Scale := {
  forward := Float.sqrt,
  inverse := fun x => x * x,
  name := "sqrt"
}

/-- Power scale with specified exponent -/
def powerScale (exponent : Float) : Scale := {
  forward := fun x => if x >= 0 then x ^ exponent else -((-x) ^ exponent),
  inverse := fun y => if y >= 0 then y ^ (1 / exponent) else -((-y) ^ (1 / exponent)),
  name := s!"pow{exponent}"
}

/-- Symlog scale (handles zero and negative values better than log) -/
def symlogScale (C : Float := 1.0) : Scale := {
  forward := fun x => Float.sign x * Float.log (1 + Float.abs x / C),
  inverse := fun y => Float.sign y * C * (Float.exp (Float.abs y) - 1),
  name := s!"symlog{C}"
}

/-- Apply a scale transformation to an array of values -/
def applyScale (scale : Scale) (values : Array Float) : Array Float :=
  values.map scale.forward

/-- Transform a function using a scale on x and/or y axes -/
def transformFunction {β} [ToFloat β]
    (f : Float → β)
    (xScale : Scale := linearScale)
    (yScale : Scale := linearScale) : Float → Float :=
  fun x => yScale.forward (toFloat (f (xScale.inverse x)))

/-- Normalize values to [0, 1] range -/
def normalize (values : Array Float) : Array Float :=
  if values.isEmpty then #[]
  else
    let min := values.foldl min values[0]!
    let max := values.foldl max values[0]!
    let range := max - min
    if range == 0 then values.map (fun _ => 0.5)
    else values.map (fun v => (v - min) / range)

/-- Standardize values (zero mean, unit variance) -/
def standardize (values : Array Float) : Array Float :=
  if values.isEmpty then #[]
  else
    let n := values.size.toFloat
    let mean := values.foldl (· + ·) 0 / n
    let variance := values.foldl (fun acc v => acc + (v - mean) ^ 2) 0 / n
    let stdDev := Float.sqrt variance
    if stdDev == 0 then values.map (fun _ => 0)
    else values.map (fun v => (v - mean) / stdDev)

/-- Clamp values to a specified range -/
def clamp (min max : Float) (values : Array Float) : Array Float :=
  values.map (fun v => if v < min then min else if v > max then max else v)

/-- Smooth data using a simple moving average -/
def smoothMovingAverage (windowSize : Nat) (values : Array Float) : Array Float :=
  if windowSize == 0 || values.size < windowSize then values
  else
    let halfWindow := windowSize / 2
    values.mapIdx fun i _ =>
      let startIdx := if i < halfWindow then 0 else i - halfWindow
      let endIdx := if i + halfWindow >= values.size then values.size - 1 else i + halfWindow
      let windowVals := (List.range (endIdx - startIdx + 1)).toArray.map (fun j => values[startIdx + j]!)
      windowVals.foldl (· + ·) 0 / windowVals.size.toFloat

end LeanPlot.Transform
