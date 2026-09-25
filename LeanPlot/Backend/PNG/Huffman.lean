/-
Huffman code construction for DEFLATE (RFC 1951 §3.2.2).

* `codeLengths` builds a Huffman tree with the two-queue method, reads off the
  leaf depths and, if the depth exceeds the limit (15 for literal/length and
  distance codes, 7 for the code-length code), redistributes lengths with the
  Kraft-sum repair used by miniz (`tdefl_huffman_enforce_max_code_size`). The
  result is always a *complete* prefix code: a lone symbol gets a phantom
  partner so that strict decoders accept the table.
* `canonicalCodes` assigns canonical codes and returns them bit-reversed,
  ready for an LSB-first bit writer.
-/

namespace LeanPlot.PNG.Huffman

/-- Add `d` to `a[i]` (no-op out of range). -/
@[inline] def bump (a : Array Nat) (i : Nat) (d : Nat := 1) : Array Nat :=
  a.modify i (· + d)

/-- Huffman code lengths for the symbol frequencies `freq`, limited to `limit`
bits. Zero-frequency symbols get length 0. The code is always complete: if
only one symbol has a nonzero frequency, a second symbol receives length 1 as
well. -/
def codeLengths (freq : Array Nat) (limit : Nat) : Array Nat := Id.run do
  let n := freq.size
  -- used symbols sorted by (freq, index) ascending
  let mut used : Array Nat := #[]
  for i in [0:n] do
    if freq[i]! > 0 then used := used.push i
  let syms := used.qsort fun a b =>
    let fa := freq[a]!; let fb := freq[b]!
    fa < fb || (fa == fb && a < b)
  let m := syms.size
  let mut lens : Array Nat := Array.replicate n 0
  if m == 0 then return lens
  if m == 1 then
    let s := syms[0]!
    lens := lens.set! s 1
    -- phantom partner keeps the code complete
    let partner := if s == 0 then 1 else 0
    if partner < n then lens := lens.set! partner 1
    return lens
  -- two-queue Huffman: leaves 0..m-1 (sorted), internal nodes m..2m-2
  let total := 2 * m - 1
  let mut w : Array Nat := Array.replicate total 0
  let mut parent : Array Nat := Array.replicate total 0
  for i in [0:m] do
    w := w.set! i freq[syms[i]!]!
  let mut i1 := 0      -- next leaf
  let mut i2 := m      -- next unconsumed internal node
  for k in [0:m-1] do
    let node := m + k
    -- pick two smallest
    let mut picks : Array Nat := #[]
    for _ in [0:2] do
      let takeLeaf := i1 < m && (i2 ≥ node || w[i1]! ≤ w[i2]!)
      if takeLeaf then
        picks := picks.push i1; i1 := i1 + 1
      else
        picks := picks.push i2; i2 := i2 + 1
    let a := picks[0]!; let b := picks[1]!
    w := w.set! node (w[a]! + w[b]!)
    parent := parent.set! a node
    parent := parent.set! b node
  -- depths: parents always have larger indices than their children
  let mut depth : Array Nat := Array.replicate total 0
  let mut j := total - 1
  while j > 0 do
    j := j - 1
    depth := depth.set! j (depth[parent[j]!]! + 1)
  let mut maxDepth := 0
  for i in [0:m] do
    maxDepth := max maxDepth depth[i]!
  let top := max maxDepth limit
  let mut blCount : Array Nat := Array.replicate (top + 1) 0
  for i in [0:m] do
    blCount := bump blCount depth[i]!
  if maxDepth > limit then
    -- fold overflow into `limit`, then repair the Kraft sum
    for d in [limit+1:top+1] do
      blCount := (blCount.set! limit (blCount[limit]! + blCount[d]!)).set! d 0
    let mut totalK : Nat := 0
    for d in [1:limit+1] do
      totalK := totalK + (blCount[d]! <<< (limit - d))
    let mut fuel := totalK
    while totalK > (1 <<< limit) && fuel > 0 do
      fuel := fuel - 1
      blCount := blCount.set! limit (blCount[limit]! - 1)
      let mut d := limit - 1
      let mut done := false
      while d > 0 && !done do
        if blCount[d]! > 0 then
          blCount := blCount.set! d (blCount[d]! - 1)
          blCount := blCount.set! (d+1) (blCount[d+1]! + 2)
          done := true
        else
          d := d - 1
      totalK := totalK - 1
  -- assign: least frequent symbols get the longest codes
  let mut idx := 0
  let mut len := min top limit
  while len > 0 do
    for _ in [0:blCount[len]!] do
      if idx < m then
        lens := lens.set! syms[idx]! len
        idx := idx + 1
    len := len - 1
  return lens

/-- Reverse the low `len` bits of `code`. -/
def reverseBits (code : Nat) (len : Nat) : Nat := Id.run do
  let mut r := 0
  let mut c := code
  for _ in [0:len] do
    r := (r <<< 1) ||| (c &&& 1)
    c := c >>> 1
  return r

/-- Canonical codes (RFC 1951 §3.2.2) for the given code lengths, each
bit-reversed for LSB-first emission. Unused symbols get code 0. -/
def canonicalCodes (lens : Array Nat) : Array UInt32 := Id.run do
  let maxLen := lens.foldl max 0
  let mut blCount : Array Nat := Array.replicate (maxLen + 1) 0
  for l in lens do
    if l > 0 then blCount := bump blCount l
  let mut next : Array Nat := Array.replicate (maxLen + 2) 0
  let mut code := 0
  for b in [1:maxLen+1] do
    code := (code + blCount[b-1]!) <<< 1
    next := next.set! b code
  let mut out : Array UInt32 := Array.replicate lens.size 0
  for s in [0:lens.size] do
    let l := lens[s]!
    if l > 0 then
      let c := next[l]!
      next := next.set! l (c + 1)
      out := out.set! s (reverseBits c l).toUInt32
  return out

/-- Is the prefix code with these lengths complete (Kraft sum exactly 1)? -/
def isComplete (lens : Array Nat) : Bool :=
  let maxLen := lens.foldl max 0
  if maxLen == 0 then false else
  let s := lens.foldl (fun acc l => if l > 0 then acc + (1 <<< (maxLen - l)) else acc) 0
  s == 1 <<< maxLen

end LeanPlot.PNG.Huffman
