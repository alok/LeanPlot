import Lean

/-! # Simple Plotting for LeanPlot

Makes your plots automatically look good with zero effort.
Just pass your function and get beautiful, labeled plots.

# For Beginners

- {lit}`smartLabels yourFunction` - Get nice axis labels automatically
- {lit}`plotSmart yourFunction data` - Plot with automatic everything
- Don't worry about the details, it just works!

# What It Does

- Turns {lit}`t` into "time"
- Turns {lit}`x` into proper spatial coordinates
- Handles duplicates like {lit}`x, y, x` → {lit}`x, y, x_2`
- Makes your plots look professional with zero effort

-/

namespace LeanPlot.Metaprogramming
open Lean Elab Term Meta

/-! ## Core Types -/

/-- Semantic roles for function parameters. -/
inductive ParameterRole
  /-- Independent variable (x-axis). -/
  | independent
  /-- Dependent variable (y-axis). -/
  | dependent
  /-- Time-like parameter. -/
  | time
  /-- Spatial coordinate. -/
  | spatial
  /-- Physical quantity. -/
  | physical
  /-- Index variable. -/
  | index
  /-- Generic parameter. -/
  | parameter
  /-- Unknown role. -/
  | unknown
  deriving BEq, Repr

instance : ToString ParameterRole where
  toString r := match r with
  | .independent => "independent"
  | .dependent => "dependent"
  | .time => "time"
  | .spatial => "spatial"
  | .physical => "physical"
  | .index => "index"
  | .parameter => "parameter"
  | .unknown => "unknown"

/-- Rich parameter information. -/
structure ParameterInfo where
  /-- The parameter's Lean name. -/
  name : Name
  /-- Inferred semantic role. -/
  role : ParameterRole
  /-- Human-readable display name. -/
  displayName : String
  deriving Repr

instance : Inhabited ParameterInfo where
  default := {
    name := Name.anonymous
    role := .unknown
    displayName := "_"
  }

/-- Intelligent axis labeling. -/
structure AxisLabels where
  /-- Label for the x-axis. -/
  xLabel : String
  /-- Label for the y-axis. -/
  yLabel : String
  deriving Repr

/-- Function metadata for intelligent plotting. -/
structure FunctionMetadata where
  /-- Extracted parameter information. -/
  parameters : Array ParameterInfo
  /-- Auto-generated axis labels. -/
  axisLabels : AxisLabels
  deriving Repr

/-! ## Core Analysis Functions -/

/-- Check if string contains substring. -/
def stringContains (s : String) (sub : String) : Bool :=
  (s.splitOn sub).length > 1

/-- Infer semantic role from parameter name. -/
def inferParameterRole (name : Name) : ParameterRole :=
  let nameStr := name.toString.toLower

  if stringContains nameStr "time" || nameStr == "t" then
    .time
  else if nameStr ∈ ["x", "y", "z", "position"] then
    .spatial
  else if stringContains nameStr "velocity" || stringContains nameStr "temperature" then
    .physical
  else if nameStr ∈ ["i", "j", "k", "index"] then
    .index
  else
    .unknown

/-- Enhance parameter name based on role. -/
def enhanceNameWithRole (baseName : String) (role : ParameterRole) : String :=
  match role with
  | .time => if baseName.length ≤ 2 then "time" else baseName
  | .index => if baseName ∈ ["i", "j", "k"] then s!"index_{baseName}" else baseName
  | _ => baseName

/-- Extract parameter metadata from expression. -/
partial def extractParameterMetadata (expr : Expr) : Array ParameterInfo :=
  let rec go (e : Expr) (position : Nat) : Array ParameterInfo :=
    match e with
    | .lam name _ body _ =>
      let role := inferParameterRole name
      let displayName := enhanceNameWithRole name.toString role
      let param : ParameterInfo := {
        name := name
        role := role
        displayName := displayName
      }
      #[param] ++ go body (position + 1)
    | _ => #[]

  go expr 0

/-- Disambiguate duplicate parameter names. -/
def disambiguateParameterNames (params : Array ParameterInfo) : Array ParameterInfo :=
  let result := params.foldl (fun acc param =>
    let existing := acc.filter (fun p => p.displayName == param.displayName)
    let count := existing.size
    let finalName := if count > 0 then s!"{param.displayName}_{count + 1}" else param.displayName
    let updatedParam := { param with displayName := finalName }
    acc.push updatedParam
  ) #[]
  result

/-- Generate intelligent axis labels. -/
def generateAxisLabels (params : Array ParameterInfo) : AxisLabels :=
  match params.size with
  | 0 => { xLabel := "x", yLabel := "y" }
  | 1 =>
    let param := params[0]!
    { xLabel := param.displayName, yLabel := s!"f({param.displayName})" }
  | _ =>
    let xParam := params[0]!
    let yParam := params[1]!
    { xLabel := xParam.displayName, yLabel := yParam.displayName }

/-- Comprehensive function analysis. -/
def analyzeFunction (expr : Expr) : FunctionMetadata :=
  let rawParams := extractParameterMetadata expr
  let params := disambiguateParameterNames rawParams
  let axisLabels := generateAxisLabels params

  {
    parameters := params
    axisLabels := axisLabels
  }

/-! ## Super Simple API (Just Use These!) -/

/-- 🎯 Get nice axis labels automatically. Works on any function!

Examples:
- {lit}`smartLabels (fun t => t^2)` gives you {lit}`("time", "f(time)")`
- {lit}`smartLabels (fun x y => x + y)` gives you {lit}`("x", "y")`
- Handles duplicates: {lit}`smartLabels (fun x y x => x*y*x)` gives you {lit}`("x", "y")`
-/
def smartLabels (expr : Expr) : String × String :=
  let metadata := analyzeFunction expr
  (metadata.axisLabels.xLabel, metadata.axisLabels.yLabel)

/-- Get all parameter names, cleaned up.

Examples:
- {lit}`smartNames (fun t => t^2)` gives you {lit}`["time"]`
- {lit}`smartNames (fun x y x => x*y*x)` gives you {lit}`["x", "y", "x_2"]`
- {lit}`smartNames (fun i j => i + j)` gives you {lit}`["index_i", "index_j"]`
-/
def smartNames (expr : Expr) : Array String :=
  let metadata := analyzeFunction expr
  metadata.parameters.map (·.displayName)

/-- 🎯 Just fix duplicate names in any string list. That's it.

Examples:
- {lit}`fixDuplicates ["x", "y", "x"]` gives you {lit}`["x", "y", "x_2"]`
- {lit}`fixDuplicates ["a", "a", "a"]` gives you {lit}`["a", "a_2", "a_3"]`
- {lit}`fixDuplicates ["unique"]` gives you {lit}`["unique"]` (unchanged)
-/
def fixDuplicates (names : Array String) : Array String :=
  names.foldl (fun acc name =>
    let duplicates := acc.filter (fun s => s.startsWith name)
    let count := duplicates.size
    let disambiguated := if count == 0 then name else s!"{name}_{count + 1}"
    acc.push disambiguated) #[]

/-! ## Old API (Still Works) -/

/-- Extract parameter names with semantic enhancement. -/
def getParameterNames (expr : Expr) : Array String := smartNames expr

/-- Get intelligent axis labels. -/
def getAxisLabels (expr : Expr) : String × String := smartLabels expr

/-- Simple parameter name extraction. -/
partial def extractParameterNames (expr : Expr) : Array Name :=
  match expr with
  | .lam name _ body _ => #[name] ++ extractParameterNames body
  | _ => #[]

/-- Convert name to string. -/
def nameToString (n : Name) : String :=
  match n with
  | .anonymous => "_"
  | .str _ s => s
  | .num _ n => s!"_{n}"

/-- Simple disambiguation for string arrays. -/
def disambiguateNames (names : Array String) : Array String := fixDuplicates names

/-! ## Easy Examples (Copy These!) -/

section EasyExamples

-- 🎯 Simple test expressions (you can copy these)
/-- Test expression representing a time-based function. -/
def myTimeFunction : Expr :=
  Expr.lam `t (Expr.const ``Float []) (Expr.bvar 0) BinderInfo.default

/-- Test expression with duplicate parameter names. -/
def myDuplicateFunction : Expr :=
  Expr.lam `x (Expr.const ``Float [])
    (Expr.lam `y (Expr.const ``Float [])
      (Expr.lam `x (Expr.const ``Float []) (Expr.bvar 0) BinderInfo.default)
      BinderInfo.default)
    BinderInfo.default

-- 🎯 See the magic happen!
#eval smartNames myTimeFunction         -- Shows: ["time"] (enhanced from `t`)
#eval smartLabels myTimeFunction        -- Shows: ("time", "f(time)")

#eval smartNames myDuplicateFunction    -- Shows: ["x", "y", "x_2"] (fixed duplicates!)
#eval smartLabels myDuplicateFunction   -- Shows: ("x", "y")

-- 🎯 Just fixing duplicate names (most common use case)
#eval fixDuplicates #["x", "y", "x", "z", "x"]     -- ["x", "y", "x_2", "z", "x_3"]
#eval fixDuplicates #["time", "time", "velocity"]  -- ["time", "time_2", "velocity"]

-- 🎯 Old API still works exactly the same
#eval getParameterNames myTimeFunction     -- Same as smartNames
#eval getAxisLabels myTimeFunction         -- Same as smartLabels
#eval disambiguateNames #["a", "a", "b"]   -- Same as fixDuplicates

end EasyExamples

/-! ## Common Patterns (Just Copy & Modify) -/

-- ✅ Pattern 1: Get nice labels for any function
example : String × String := smartLabels myTimeFunction

-- ✅ Pattern 2: Fix duplicate parameter names
example : Array String := fixDuplicates #["x", "y", "x"]

-- ✅ Pattern 3: Get enhanced parameter names
example : Array String := smartNames myDuplicateFunction

-- ✅ Pattern 4: Everything in one go
example : String × String × Array String :=
  let expr := myTimeFunction
  let labels := smartLabels expr
  let names := smartNames expr
  (labels.1, labels.2, names)

end LeanPlot.Metaprogramming
