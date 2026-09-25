import LeanPlot.Backend.PNG.Checksum
import LeanPlot.Backend.PNG.Huffman
import LeanPlot.Backend.PNG.Deflate
import LeanPlot.Backend.PNG.Inflate
import LeanPlot.Backend.PNG.Filter
import LeanPlot.Backend.PNG.Encode
import LeanPlot.Backend.PNG.Decode
import LeanPlot.Util.Base64

/-!
# PNG backend

Zero-dependency PNG codec:

* `LeanPlot.PNG.encode` / `encodeRGBA` / `encodeRGB` / `encodeAuto`: adaptive
  per-row filters plus DEFLATE (LZ77 with hash chains and lazy matching, and
  stored, fixed or dynamic Huffman blocks, whichever is smallest per block).
* `LeanPlot.PNG.decode` / `decodeRGBA`: a minimal decoder for our own output,
  used by the tests.
* `LeanPlot.PNG.dataURI`: a base64 `data:` URI for SVG `<image>` embedding.
-/
