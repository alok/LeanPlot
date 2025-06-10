# Metaprogramming Features in LeanPlot

LeanPlot includes advanced metaprogramming capabilities that allow automatic extraction of parameter names from function expressions for intelligent axis labeling.

## Clean, Simple API

The API uses ordinary functions rather than special syntax, making it more composable and easier to use.

### 1. Basic Parameter Name Extraction

Extract parameter names from expressions using `getParameterNames`:

```lean
open LeanPlot.Metaprogramming Lean

-- Simple expression
def simpleExpr : Expr := Expr.lam `time (Expr.const ``Float []) (Expr.bvar 0) BinderInfo.default
#eval getParameterNames simpleExpr
-- Result: #["time"]
```

### 2. Automatic Axis Label Generation

Generate axis labels automatically with `getAxisLabels`:

```lean
#eval getAxisLabels simpleExpr
-- Result: ("time", "y")
```

### 3. Duplicate Name Disambiguation

Handle duplicate parameter names automatically:

```lean
-- Expression with duplicate parameters
def duplicateExpr : Expr := 
  Expr.lam `x (Expr.const ``Float [])
    (Expr.lam `y (Expr.const ``Float [])
      (Expr.lam `x (Expr.const ``Float []) (Expr.bvar 0) BinderInfo.default)
      BinderInfo.default)
    BinderInfo.default

#eval getParameterNames duplicateExpr
-- Result: #["x", "y", "x_2"]

#eval getAxisLabels duplicateExpr  
-- Result: ("x", "y")
```

### 4. Direct Array Operations

Work directly with arrays for better performance:

```lean
#eval disambiguateNames #["x", "y", "x", "z", "x", "y"]
-- Result: #["x", "y", "x_2", "z", "x_3", "y_2"]
```

## Implementation Details

### Core Functions

- `extractParameterNames`: Recursively extracts parameter names from lambda expressions into Arrays
- `getParameterNames`: Combines extraction with disambiguation
- `getAxisLabels`: Generates (xLabel, yLabel) tuples from parameter names
- `disambiguateNames`: Adds suffixes to duplicate names (e.g., "x", "x_2", "x_3")

### Disambiguation Algorithm

1. **First pass**: Count occurrences of each parameter name
2. **Second pass**: Add suffixes to duplicates:
   - First occurrence: keeps original name
   - Subsequent occurrences: get suffixes `_2`, `_3`, etc.

### Example Disambiguation

```lean
-- Input: ["x", "y", "x", "z", "x", "y"]
-- Output: ["x", "y", "x_2", "z", "x_3", "y_2"]
```

## Usage in Plotting

These features enable more ergonomic plotting by automatically generating meaningful axis labels:

```lean
-- Current: manual labels
mkLineChartWithLabels data series (some "time") (some "position")

-- Enhanced with metaprogramming
def mkLineChartWithAutoLabels (data : Array Json) (expr : Expr) (series : Array (String × String)) : Html :=
  let labels := getAxisLabels expr
  mkLineChartWithLabels data series (some labels.1) (some labels.2)
```

## Technical Foundation

The implementation uses Lean 4's metaprogramming system:

- **Expression Analysis**: Pattern matching on `Expr.lam` constructors
- **Name Extraction**: Using `bindingName` to get parameter names
- **Term Elaboration**: Custom `TermElab` functions for compile-time evaluation
- **Type Safety**: Full Lean 4 type checking with universe level handling

## Test Coverage

Comprehensive test cases cover:

- Basic parameter extraction
- Multiple parameter functions
- Duplicate name handling
- Edge cases (anonymous parameters, numbered names)
- Empty and single-parameter functions

See [`LeanPlot/Test/DuplicateNamesTest.lean`](../LeanPlot/Test/DuplicateNamesTest.lean) for complete test suite.
