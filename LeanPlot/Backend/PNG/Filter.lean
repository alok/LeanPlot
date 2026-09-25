/-
PNG scanline filters (PNG spec §9).

`filterRows` picks, per row, the filter type with the smallest sum of
absolute (signed-byte) residuals, the heuristic libpng recommends. All five
residual sums are computed in one pass over the row. `unfilterRows` is the
inverse, used by the test decoder.
-/

namespace LeanPlot.PNG.Filter

/-- PNG filter types. -/
inductive Kind where
  | none | sub | up | average | paeth
  deriving Repr, Inhabited, BEq, DecidableEq

/-- Filter selection policy. -/
inductive Strategy where
  /-- Per-row minimum sum of absolute residuals (default). -/
  | adaptive
  /-- The same filter for every row. -/
  | fixed (k : Kind)
  deriving Repr, Inhabited

/-- Filter type byte. -/
def Kind.toUInt8 : Kind → UInt8
  | none => 0 | sub => 1 | up => 2 | average => 3 | paeth => 4

/-- `|x - y|` on `UInt16`. -/
@[inline] def absDiff (x y : UInt16) : UInt16 := if x ≥ y then x - y else y - x

/-- Paeth predictor (`p = a + b - c`; pick the neighbour closest to `p`, ties
`a`, then `b`). Uses `p - a = b - c`, `p - b = a - c`, `p - c = a + b - 2c`. -/
@[inline] def paeth (a b c : UInt8) : UInt8 :=
  let a' := a.toUInt16; let b' := b.toUInt16; let c' := c.toUInt16
  let pa := absDiff b' c'
  let pb := absDiff a' c'
  let pc := absDiff (a' + b') (c' + c')
  if pa ≤ pb && pa ≤ pc then a else if pb ≤ pc then b else c

/-- |v| when `v` is read as a signed byte. -/
@[inline] def absSigned (v : UInt8) : UInt64 :=
  if v < 128 then v.toUInt64 else 256 - v.toUInt64

/-- Residual sums of the five filters for the row at `base` (previous row at
`prevBase`, absent if `first`). -/
def scoreRow (raw : ByteArray) (base prevBase rowLen bpp : Nat) (first : Bool) :
    UInt64 × UInt64 × UInt64 × UInt64 × UInt64 :=
  let rec go (j : Nat) (s0 s1 s2 s3 s4 : UInt64) : UInt64 × UInt64 × UInt64 × UInt64 × UInt64 :=
    if j < rowLen then
      let x := raw.get! (base + j)
      let a : UInt8 := if j ≥ bpp then raw.get! (base + j - bpp) else 0
      let b : UInt8 := if first then 0 else raw.get! (prevBase + j)
      let c : UInt8 := if first || j < bpp then 0 else raw.get! (prevBase + j - bpp)
      let avg : UInt8 := ((a.toUInt16 + b.toUInt16) >>> 1).toUInt8
      go (j + 1) (s0 + absSigned x) (s1 + absSigned (x - a)) (s2 + absSigned (x - b))
        (s3 + absSigned (x - avg)) (s4 + absSigned (x - paeth a b c))
    else (s0, s1, s2, s3, s4)
  termination_by rowLen - j
  go 0 0 0 0 0 0

/-- Append the row at `base` filtered with `k`. -/
def writeRow (out raw : ByteArray) (base prevBase rowLen bpp : Nat) (first : Bool) (k : Kind) : ByteArray :=
  let out := out.push k.toUInt8
  let rec go (j : Nat) (out : ByteArray) : ByteArray :=
    if j < rowLen then
      let x := raw.get! (base + j)
      let a : UInt8 := if j ≥ bpp then raw.get! (base + j - bpp) else 0
      let b : UInt8 := if first then 0 else raw.get! (prevBase + j)
      let v : UInt8 := match k with
        | .none => x
        | .sub => x - a
        | .up => x - b
        | .average => x - ((a.toUInt16 + b.toUInt16) >>> 1).toUInt8
        | .paeth =>
          let c : UInt8 := if first || j < bpp then 0 else raw.get! (prevBase + j - bpp)
          x - paeth a b c
      go (j + 1) (out.push v)
    else out
  termination_by rowLen - j
  go 0 out

/-- Choose the filter for one row. -/
def chooseKind (raw : ByteArray) (base prevBase rowLen bpp : Nat) (first : Bool) : Kind :=
  let (s0, s1, s2, s3, s4) := scoreRow raw base prevBase rowLen bpp first
  let (k, m) := if s1 < s0 then (Kind.sub, s1) else (Kind.none, s0)
  let (k, m) := if s2 < m then (Kind.up, s2) else (k, m)
  let (k, m) := if s3 < m then (Kind.average, s3) else (k, m)
  if s4 < m then .paeth else k

/-- Filter `h` rows of `rowLen` bytes each (`bpp` bytes per pixel) into the
PNG scanline stream (`h * (rowLen + 1)` bytes). -/
def filterRows (raw : ByteArray) (rowLen bpp h : Nat) (strategy : Strategy := .adaptive) : ByteArray :=
  let rec go (y : Nat) (out : ByteArray) : ByteArray :=
    if y < h then
      let base := y * rowLen
      let prevBase := base - rowLen
      let first := y == 0
      let k := match strategy with
        | .adaptive => chooseKind raw base prevBase rowLen bpp first
        | .fixed k => k
      go (y + 1) (writeRow out raw base prevBase rowLen bpp first k)
    else out
  termination_by h - y
  go 0 (ByteArray.emptyWithCapacity (h * (rowLen + 1))).markLinear

/-! ### Layout -/

/-- `writeRow.go` appends one byte per remaining column. -/
theorem size_writeRow_go (raw : ByteArray) (base prevBase rowLen bpp : Nat) (first : Bool) (k : Kind)
    (j : Nat) (out : ByteArray) :
    (writeRow.go raw base prevBase rowLen bpp first k j out).size = out.size + (rowLen - j) := by
  fun_induction writeRow.go raw base prevBase rowLen bpp first k j out
  · rename_i ih; rw [ih]; simp only [ByteArray.size_push]; omega
  · omega

/-- A filtered row is its filter-type byte plus `rowLen` bytes. -/
theorem size_writeRow (out raw : ByteArray) (base prevBase rowLen bpp : Nat) (first : Bool) (k : Kind) :
    (writeRow out raw base prevBase rowLen bpp first k).size = out.size + (rowLen + 1) := by
  simp only [writeRow, size_writeRow_go, ByteArray.size_push]; omega

/-- `filterRows.go` appends `rowLen + 1` bytes per remaining row. -/
theorem size_filterRows_go (raw : ByteArray) (rowLen bpp h : Nat) (strategy : Strategy) (y : Nat) (out : ByteArray) :
    (filterRows.go raw rowLen bpp h strategy y out).size = out.size + (h - y) * (rowLen + 1) := by
  fun_induction filterRows.go raw rowLen bpp h strategy y out
  · rename_i y _ hy _ _ _ _ ih
    rw [ih, size_writeRow]
    have : h - y = (h - (y + 1)) + 1 := by omega
    rw [this, Nat.add_mul]; omega
  · rename_i hy
    rw [Nat.sub_eq_zero_of_le (by omega), Nat.zero_mul, Nat.add_zero]

/-- The scanline stream has the size the PNG spec requires, `h * (rowLen + 1)`
(PNG §7.2), for every filter strategy. -/
theorem size_filterRows (raw : ByteArray) (rowLen bpp h : Nat) (strategy : Strategy) :
    (filterRows raw rowLen bpp h strategy).size = h * (rowLen + 1) := by
  rw [filterRows, size_filterRows_go]
  show 0 + (h - 0) * (rowLen + 1) = _
  rw [Nat.zero_add, Nat.sub_zero]

/-- Undo the filters of a scanline stream (`h * (rowLen+1)` bytes). -/
def unfilterRows (scan : ByteArray) (rowLen bpp h : Nat) : Except String ByteArray :=
  if scan.size < h * (rowLen + 1) then .error "png: scanline data too short" else
  let rec row (y : Nat) (out : ByteArray) : Except String ByteArray :=
    if y < h then
      let src := y * (rowLen + 1)
      let ty := scan.get! src
      if ty > 4 then .error s!"png: bad filter type {ty}" else
      let base := y * rowLen
      let first := y == 0
      let rec px (j : Nat) (out : ByteArray) : ByteArray :=
        if j < rowLen then
          let x := scan.get! (src + 1 + j)
          let a : UInt8 := if j ≥ bpp then out.get! (base + j - bpp) else 0
          let b : UInt8 := if first then 0 else out.get! (base - rowLen + j)
          let c : UInt8 := if first || j < bpp then 0 else out.get! (base - rowLen + j - bpp)
          let v : UInt8 :=
            if ty == 0 then x else if ty == 1 then x + a else if ty == 2 then x + b
            else if ty == 3 then x + ((a.toUInt16 + b.toUInt16) >>> 1).toUInt8
            else x + paeth a b c
          px (j + 1) (out.push v)
        else out
      termination_by rowLen - j
      row (y + 1) (px 0 out)
    else .ok out
  termination_by h - y
  row 0 (ByteArray.emptyWithCapacity (h * rowLen))

end LeanPlot.PNG.Filter
