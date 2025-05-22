import Lean
import LeanPlot

/--
Entry point for the `leanplot` executable.  Currently prints a friendly
*hello world* style greeting to verify the project builds and that the
binary wiring works correctly.
-/

def main : IO Unit :=
  IO.println s!"Hello, {hello}!"

