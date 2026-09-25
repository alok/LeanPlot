import LeanPlot.Backend.PNG.Checksum
import LeanPlot.Backend.PNG.Huffman

/-
DEFLATE compressor (RFC 1951) and zlib wrapper (RFC 1950).

* LZ77 with a 32 KiB window, 15-bit hash of the next three bytes, hash chains
  (`head`/`prev`) and zlib-style *lazy matching* (a match found at `i` is
  deferred by one byte in case `i+1` starts a longer one).
* Tokens are buffered per block (default 16 Ki tokens). Each block is emitted
  as whichever of stored / fixed-Huffman / dynamic-Huffman is smallest, costed
  exactly from the token frequencies.
* Hot loops are tail-recursive with every buffer passed as its own argument,
  so each `set!`/`push` is in place.
-/

namespace LeanPlot.PNG.Deflate

open LeanPlot.PNG.Huffman

/-! ## Bit writer -/

/-- LSB-first bit writer. Invariant: `n < 32` between calls. -/
structure BitWriter where
  out : ByteArray
  bits : UInt64 := 0
  n : UInt64 := 0

namespace BitWriter

/-- Append the low `k ≤ 32` bits of `v` (which must have no higher bits set). -/
@[inline] def put (w : BitWriter) (v : UInt64) (k : UInt64) : BitWriter :=
  let bits := w.bits ||| (v <<< w.n)
  let n := w.n + k
  if n < 32 then { out := w.out, bits, n }
  else
    let out := (((w.out.push bits.toUInt8).push (bits >>> 8).toUInt8).push (bits >>> 16).toUInt8).push (bits >>> 24).toUInt8
    { out, bits := bits >>> 32, n := n - 32 }

/-- Pad with zero bits to a byte boundary and flush. -/
def align (w : BitWriter) : BitWriter :=
  let rec go (out : ByteArray) (bits n : UInt64) (fuel : Nat) : ByteArray :=
    match fuel with
    | 0 => out
    | f + 1 => if n == 0 then out else go (out.push bits.toUInt8) (bits >>> 8) (if n ≥ 8 then n - 8 else 0) f
  { out := go w.out w.bits w.n 8, bits := 0, n := 0 }

/-- Append raw bytes `src[start, start+len)`; the writer must be aligned. -/
def appendAligned (w : BitWriter) (src : ByteArray) (start len : Nat) : BitWriter :=
  { w with out := src.copySlice start w.out w.out.size len false }

end BitWriter

/-! ## Static tables -/

/-- Base match length of length codes 257‥285. -/
def lenBase : Array Nat :=
  #[3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115,
    131, 163, 195, 227, 258]
/-- Extra bits of length codes 257‥285. -/
def lenExtra : Array Nat :=
  #[0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0]
/-- Base distance of distance codes 0‥29. -/
def distBase : Array Nat :=
  #[1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537,
    2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577]
/-- Extra bits of distance codes 0‥29. -/
def distExtra : Array Nat :=
  #[0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13]

/-- `lenCodeTable[len]` = length-code index (0‥28, i.e. symbol − 257) for `3 ≤ len ≤ 258`. -/
def lenCodeTable : ByteArray := Id.run do
  let mut t := ByteArray.mk (Array.replicate 259 0)
  for k in [0:29] do
    let b := lenBase[k]!
    for j in [0:(1 <<< lenExtra[k]!)] do
      if b + j ≤ 258 then t := t.set! (b + j) k.toUInt8
  -- 258 has its own code (285) and overrides 284's range end
  return t.set! 258 28

/-- `distCodeTable[d]` = distance code (0‥29) for `1 ≤ d ≤ 32768`. -/
def distCodeTable : ByteArray := Id.run do
  let mut t := ByteArray.mk (Array.replicate 32769 0)
  for k in [0:30] do
    let b := distBase[k]!
    for j in [0:(1 <<< distExtra[k]!)] do
      if b + j ≤ 32768 then t := t.set! (b + j) k.toUInt8
  return t

/-- Fixed literal/length code lengths (RFC 1951 §3.2.6), 288 symbols. -/
def fixedLitLens : Array Nat :=
  Array.ofFn fun (i : Fin 288) => if i.val < 144 then 8 else if i.val < 256 then 9 else if i.val < 280 then 7 else 8
/-- Fixed distance code lengths: 30 codes of 5 bits. -/
def fixedDistLens : Array Nat := Array.replicate 30 5
/-- Bit-reversed fixed literal/length codes. -/
def fixedLitCodes : Array UInt32 := canonicalCodes fixedLitLens
/-- Bit-reversed fixed distance codes. -/
def fixedDistCodes : Array UInt32 := canonicalCodes fixedDistLens

/-- Permutation in which code-length code lengths are transmitted. -/
def clOrder : Array Nat := #[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

/-! ## Compression parameters -/

/-- LZ77 effort parameters (zlib names). -/
structure Level where
  /-- Maximum hash-chain links followed per search. -/
  maxChain : Nat := 128
  /-- Quarter the chain budget once the deferred match is at least this long. -/
  goodLen : Nat := 8
  /-- Stop searching once a match this long is found. -/
  niceLen : Nat := 128
  /-- Do not look for a better match when the deferred one is at least this long. -/
  maxLazy : Nat := 16
  /-- Tokens buffered per block. -/
  blockTokens : Nat := 16384
  /-- Matches longer than this are not hashed position by position (zlib's
  `max_insert_length`); long runs are then skipped in O(1). -/
  maxInsert : Nat := 16
  deriving Repr, Inhabited

namespace Level
/-- Fast: short chains. -/
def fast : Level := { maxChain := 8, goodLen := 4, niceLen := 32, maxLazy := 4, maxInsert := 8 }
/-- zlib level 6 equivalent. -/
def default : Level := {}
/-- zlib level 9 equivalent. -/
def best : Level := { maxChain := 4096, goodLen := 32, niceLen := 258, maxLazy := 258, maxInsert := 258 }
end Level

/-! ## Tokens

A token is a `UInt32`: a literal byte `b` is `b`; a match is
`0x80000000 ||| (len <<< 16) ||| dist` with `3 ≤ len ≤ 258`, `1 ≤ dist ≤ 32767`. -/

/-- Encode a match token. -/
@[inline] def mkMatch (len dist : Nat) : UInt32 := (0x80000000 : UInt32) ||| (len.toUInt32 <<< 16) ||| dist.toUInt32
/-- Is the token a match (rather than a literal)? -/
@[inline] def isMatch (t : UInt32) : Bool := t &&& 0x80000000 != 0
/-- Match length of a match token. -/
@[inline] def matchLen (t : UInt32) : Nat := ((t >>> 16) &&& 0x1FF).toNat
/-- Match distance of a match token. -/
@[inline] def matchDist (t : UInt32) : Nat := (t &&& 0xFFFF).toNat

/-! ## Block emission -/

/-- Run-length encode a code-length sequence with symbols 16/17/18. Each entry
is `sym + 32 * extraValue`. -/
def rleCodeLengths (lens : Array Nat) : Array Nat := Id.run do
  let n := lens.size
  let mut out : Array Nat := #[]
  let mut i := 0
  while i < n do
    let l := lens[i]!
    let mut run := 1
    while i + run < n && lens[i + run]! == l do run := run + 1
    i := i + run
    if l == 0 then
      while run ≥ 11 do
        let r := min run 138
        out := out.push (18 + 32 * (r - 11)); run := run - r
      if run ≥ 3 then
        out := out.push (17 + 32 * (run - 3)); run := 0
      for _ in [0:run] do out := out.push 0
    else
      out := out.push l; run := run - 1
      while run ≥ 3 do
        let r := min run 6
        out := out.push (16 + 32 * (r - 3)); run := run - r
      for _ in [0:run] do out := out.push l
  return out

/-- Extra bits carried by a code-length symbol. -/
@[inline] def clExtraBits (sym : Nat) : Nat :=
  if sym == 16 then 2 else if sym == 17 then 3 else if sym == 18 then 7 else 0

/-- Σ freq[s] · lens[s]. -/
def weighted (freq lens : Array Nat) : Nat := Id.run do
  let mut s := 0
  for i in [0:freq.size] do
    s := s + freq[i]! * lens[i]!
  return s

/-- Extra bits contributed by the length and distance codes. -/
def extraBits (lfreq dfreq : Array Nat) : Nat := Id.run do
  let mut s := 0
  for k in [0:29] do s := s + lfreq[257 + k]! * lenExtra[k]!
  for k in [0:30] do s := s + dfreq[k]! * distExtra[k]!
  return s

/-- Emit the token stream with the given (bit-reversed) codes and lengths. -/
def emitTokens (w : BitWriter) (toks : Array UInt32) (lcodes : Array UInt32) (llens : Array Nat)
    (dcodes : Array UInt32) (dlens : Array Nat) : BitWriter :=
  let lct := lenCodeTable
  let dct := distCodeTable
  let rec go (i : Nat) (w : BitWriter) : BitWriter :=
    if h : i < toks.size then
      let t := toks[i]
      if isMatch t then
        let len := matchLen t
        let dist := matchDist t
        let lc := (lct.get! len).toNat
        let w := w.put (lcodes[257 + lc]!).toUInt64 (llens[257 + lc]!).toUInt64
        let w := w.put (len - lenBase[lc]!).toUInt64 (lenExtra[lc]!).toUInt64
        let dc := (dct.get! dist).toNat
        let w := w.put (dcodes[dc]!).toUInt64 (dlens[dc]!).toUInt64
        go (i + 1) (w.put (dist - distBase[dc]!).toUInt64 (distExtra[dc]!).toUInt64)
      else
        let b := t.toNat
        go (i + 1) (w.put (lcodes[b]!).toUInt64 (llens[b]!).toUInt64)
    else w
  termination_by toks.size - i
  let w := go 0 w
  w.put (lcodes[256]!).toUInt64 (llens[256]!).toUInt64

/-- Write one block covering input `data[start, stop)` whose LZ77 tokens are
`toks` (frequencies `lfreq`, `dfreq`, not yet counting end-of-block). Chooses
the cheapest of stored, fixed and dynamic encodings. -/
def writeBlock (w : BitWriter) (data : ByteArray) (start stop : Nat) (toks : Array UInt32)
    (lfreq dfreq : Array Nat) (final : Bool) : BitWriter := Id.run do
  let lfreq := bump lfreq 256
  let bfinal : UInt64 := if final then 1 else 0
  -- dynamic code
  let llens := codeLengths lfreq 15
  let dlens := codeLengths dfreq 15
  -- a block with no matches still needs a (complete) distance code
  let dlens := if dlens.all (· == 0) then (dlens.set! 0 1).set! 1 1 else dlens
  let mut hlit := 286
  while hlit > 257 && llens[hlit - 1]! == 0 do hlit := hlit - 1
  let mut hdist := 30
  while hdist > 1 && dlens[hdist - 1]! == 0 do hdist := hdist - 1
  let seq := (llens.extract 0 hlit) ++ (dlens.extract 0 hdist)
  let rle := rleCodeLengths seq
  let mut clFreq : Array Nat := Array.replicate 19 0
  for e in rle do clFreq := bump clFreq (e % 32)
  let cllens := codeLengths clFreq 7
  let mut hclen := 19
  while hclen > 4 && cllens[clOrder[hclen - 1]!]! == 0 do hclen := hclen - 1
  let extra := extraBits lfreq dfreq
  let mut rleBits := 0
  for e in rle do rleBits := rleBits + cllens[e % 32]! + clExtraBits (e % 32)
  let dynBits := 3 + 14 + 3 * hclen + rleBits + weighted lfreq llens + weighted dfreq dlens + extra
  let fixBits := 3 + weighted lfreq fixedLitLens + weighted dfreq fixedDistLens + extra
  let rawLen := stop - start
  let nChunks := max 1 ((rawLen + 65534) / 65535)
  let storedBits := 3 + 7 + 32 * nChunks + 8 * rawLen + 10 * (nChunks - 1)
  if storedBits < dynBits && storedBits < fixBits then
    -- stored: split into ≤ 65535-byte chunks
    let mut w := w
    let mut pos := start
    for k in [0:nChunks] do
      let len := min 65535 (stop - pos)
      let last := k + 1 == nChunks
      w := w.put (if last then bfinal else 0) 3
      w := w.align
      w := w.put len.toUInt64 16
      w := w.put ((len.toUInt64) ^^^ 0xFFFF) 16
      w := w.appendAligned data pos len
      pos := pos + len
    return w
  else if fixBits ≤ dynBits then
    let w := w.put (bfinal ||| 2) 3
    return emitTokens w toks fixedLitCodes fixedLitLens fixedDistCodes fixedDistLens
  else
    let mut w := w.put (bfinal ||| 4) 3
    w := w.put (hlit - 257).toUInt64 5
    w := w.put (hdist - 1).toUInt64 5
    w := w.put (hclen - 4).toUInt64 4
    for k in [0:hclen] do
      w := w.put (cllens[clOrder[k]!]!).toUInt64 3
    let clcodes := canonicalCodes cllens
    for e in rle do
      let s := e % 32
      w := w.put (clcodes[s]!).toUInt64 (cllens[s]!).toUInt64
      let xb := clExtraBits s
      if xb > 0 then w := w.put (e / 32).toUInt64 xb.toUInt64
    return emitTokens w toks (canonicalCodes llens) llens (canonicalCodes dlens) dlens

/-! ## LZ77 -/

/-- Window size (distances are kept strictly below it). -/
def wsize : Nat := 32768
/-- Hash table size. -/
def hsize : Nat := 32768

/-- Hash of `data[i], data[i+1], data[i+2]`. Computed in `UInt32`:
`Nat.shiftLeft` is not inlined by the runtime and goes through GMP on
every call. -/
@[inline] def hash3 (data : ByteArray) (i : Nat) : Nat :=
  ((((data.get! i).toUInt32 <<< 10) ^^^ ((data.get! (i+1)).toUInt32 <<< 5) ^^^ (data.get! (i+2)).toUInt32)
    &&& 0x7FFF).toNat

/-- Length of the common prefix of `data[p…]` and `data[i…]`, at most `maxLen`. -/
@[inline] def commonPrefix (data : ByteArray) (p i maxLen : Nat) : Nat :=
  let rec go (k : Nat) : Nat :=
    if k < maxLen then
      if data.get! (p + k) == data.get! (i + k) then go (k + 1) else k
    else k
  termination_by maxLen - k
  go 0

/-! The hash chains live in one array `chains`: `chains[h]` for `h < hsize` is
the most recent position (plus one; `0` = none) with hash `h`, and
`chains[hsize + (p % wsize)]` links position `p` to the previous position
with the same hash. -/

/-- Walk the hash chain from candidate `cur` (position + 1, `0` = none) looking
for the longest match at `i`. Returns `bestLen * 65536 + bestDist`. -/
def searchChain (data : ByteArray) (chains : Array Nat) (i maxLen niceLen : Nat)
    (cur chain bestLen bestDist : Nat) : Nat :=
  match chain with
  | 0 => bestLen * 65536 + bestDist
  | chain + 1 =>
    if cur == 0 then bestLen * 65536 + bestDist else
    let p := cur - 1
    if p ≥ i || i - p ≥ wsize then bestLen * 65536 + bestDist else
    -- quick reject on the byte that would extend the current best
    let len :=
      if bestLen < maxLen && data.get! (p + bestLen) != data.get! (i + bestLen) then 0
      else commonPrefix data p i maxLen
    let better := len > bestLen
    let bestLen := if better then len else bestLen
    let bestDist := if better then i - p else bestDist
    if bestLen ≥ niceLen || bestLen ≥ maxLen then bestLen * 65536 + bestDist else
    let next := chains[hsize + p % wsize]!
    if next ≥ cur then bestLen * 65536 + bestDist else
    searchChain data chains i maxLen niceLen next chain bestLen bestDist

/-- Insert position `i` into the hash chains (needs `i + 2 < n`). -/
@[inline] def insertPos (data : ByteArray) (chains : Array Nat) (i : Nat) : Array Nat :=
  if i + 2 < data.size then
    let h := hash3 data i
    let chains := chains.set! (hsize + i % wsize) chains[h]!
    chains.set! h (i + 1)
  else chains

/-- Insert positions `[i, stop)`. -/
def insertRange (data : ByteArray) (chains : Array Nat) (i stop : Nat) : Array Nat :=
  if i < stop then insertRange data (insertPos data chains i) (i + 1) stop else chains
termination_by stop - i

/-- Main lazy-matching loop (zlib `deflate_slow`). `prevLen ≥ 3` with `avail`
means a match at `i - 1` is pending; `avail` alone means the literal
`data[i-1]` is pending. Tokens of the current block (which starts at input
offset `blockStart`) are buffered in `toks`/`lfreq`/`dfreq` and written to
`w` whenever the buffer fills. Matches longer than `maxInsert` are not
entered into the hash chains position by position (zlib's `max_insert_length`
shortcut), which matters for the long runs typical of plots. -/
def lzLoop (data : ByteArray) (lv : Level) (fuel i prevLen prevDist : Nat)
    (avail : Bool) (chains : Array Nat) (toks : Array UInt32) (lfreq dfreq : Array Nat)
    (blockStart : Nat) (w : BitWriter) : BitWriter :=
  let n := data.size
  match fuel with
  | 0 => writeBlock w data blockStart n toks lfreq dfreq true
  | fuel + 1 =>
  if toks.size ≥ lv.blockTokens then
    -- flush the full block; everything before `emitted` is tokenized
    let emitted := if avail then i - 1 else i
    let w := writeBlock w data blockStart emitted toks lfreq dfreq false
    lzLoop data lv fuel i prevLen prevDist avail chains
      (Array.emptyWithCapacity lv.blockTokens) (Array.replicate 286 0) (Array.replicate 30 0) emitted w
  else if i ≥ n then
    if avail then
      let b := data.get! (n - 1)
      writeBlock w data blockStart n (toks.push b.toUInt32) (bump lfreq b.toNat) dfreq true
    else writeBlock w data blockStart n toks lfreq dfreq true
  else
    let chains := insertPos data chains i
    let maxLen := min 258 (n - i)
    let packed :=
      if prevLen ≥ lv.maxLazy || maxLen < 3 then 0
      else
        let chain := if prevLen ≥ lv.goodLen then lv.maxChain / 4 + 1 else lv.maxChain
        searchChain data chains i maxLen lv.niceLen chains[hsize + i % wsize]! chain (max prevLen 2) 0
    let bl := packed / 65536
    let bd := packed % 65536
    -- a short far match costs more than three literals
    let ok := !(bl < 3 || bd == 0 || (bl == 3 && bd > 4096))
    let len := if ok then bl else 0
    let dist := if ok then bd else 0
    if prevLen ≥ 3 && len ≤ prevLen then
      -- emit the deferred match starting at i - 1
      let lc := (lenCodeTable.get! prevLen).toNat
      let dc := (distCodeTable.get! prevDist).toNat
      let stop := i - 1 + prevLen
      let chains := if prevLen ≤ lv.maxInsert then insertRange data chains (i + 1) (min stop n)
        else insertRange data chains (max (i + 1) (stop - 3)) (min stop n)
      lzLoop data lv fuel stop 0 0 false chains (toks.push (mkMatch prevLen prevDist))
        (bump lfreq (257 + lc)) (bump dfreq dc) blockStart w
    else if avail then
      let b := data.get! (i - 1)
      lzLoop data lv fuel (i + 1) len dist true chains (toks.push b.toUInt32)
        (bump lfreq b.toNat) dfreq blockStart w
    else
      lzLoop data lv fuel (i + 1) len dist true chains toks lfreq dfreq blockStart w

/-- Raw DEFLATE stream (no zlib header) of `data`. -/
def deflate (data : ByteArray) (lv : Level := .default) : ByteArray :=
  let n := data.size
  let w := lzLoop data lv (n + 2) 0 0 0 false (Array.replicate (hsize + wsize) 0).markLinear
    (Array.emptyWithCapacity lv.blockTokens) (Array.replicate 286 0) (Array.replicate 30 0) 0
    { out := (ByteArray.emptyWithCapacity (n / 4 + 64)).markLinear }
  w.align.out

/-- zlib stream (RFC 1950): header `78 9C`, DEFLATE data, big-endian Adler-32. -/
def zlibCompress (data : ByteArray) (lv : Level := .default) : ByteArray :=
  let body := deflate data lv
  let a := LeanPlot.PNG.adler32 data
  let out := (ByteArray.emptyWithCapacity (body.size + 6)).push 0x78 |>.push 0x9C
  let out := out ++ body
  (((out.push (a >>> 24).toUInt8).push (a >>> 16).toUInt8).push (a >>> 8).toUInt8).push a.toUInt8

/-- zlib stream using only stored blocks (for tests and as a size baseline). -/
def zlibStored (data : ByteArray) : ByteArray :=
  let n := data.size
  let nChunks := max 1 ((n + 65534) / 65535)
  let rec go (k pos : Nat) (w : BitWriter) : BitWriter :=
    if k < nChunks then
      let len := min 65535 (n - pos)
      let w := (w.put (if k + 1 == nChunks then 1 else 0) 3).align
      let w := (w.put len.toUInt64 16).put (len.toUInt64 ^^^ 0xFFFF) 16
      go (k + 1) (pos + len) (w.appendAligned data pos len)
    else w
  termination_by nChunks - k
  let w := go 0 0 { out := (ByteArray.emptyWithCapacity (n + 5 * nChunks + 6)).push 0x78 |>.push 0x01 }
  let a := LeanPlot.PNG.adler32 data
  let out := w.align.out
  (((out.push (a >>> 24).toUInt8).push (a >>> 16).toUInt8).push (a >>> 8).toUInt8).push a.toUInt8

end LeanPlot.PNG.Deflate
