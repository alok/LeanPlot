# LeanPlot Justfile

# Default task
_default:
	@just --list

# Build the project
build:
	lake build

# Release build (optimised)
release:
	lake build -R

# Watch build continuously
watch:
	lake build -w

# Lint (placeholder – adjust when linter chosen)
lint:
	lake env lean --run Std.Tactic.Lint

# Clean build artefacts
clean:
	lake clean

# Lean REPL (uses Lake to set env)
repl:
	lake repl

# Install git hooks declared in .githooks
hooks-install:
	git config core.hooksPath .githooks
	chmod +x .githooks/*
	@echo "git hooks path set to .githooks"

# Run overlay demo (opens infoview when using Lean4 editor)
# This is a noop terminal command, but included for discoverability
overlay:
	@echo "Open LeanPlot/Demos/OverlayPlot.lean and put cursor on #html overlay"

# Update changelog timestamp
changelog-update:
	python - <<'PY'
from datetime import datetime, timezone
stamp = datetime.now(timezone.utc).strftime('%Y-%m-%d:%H:%M')
import pathlib, re
path = pathlib.Path('CHANGELOG.md')
txt = path.read_text()
import sys, re
new = re.sub(r'\[0\.1\.0\] – [0-9-:]+', '[0.1.0] – '+stamp, txt, count=1)
path.write_text(new)
print('Timestamp updated ->', stamp)
PY

# Docs placeholder
docs:
	@echo "TODO: docs generation (e.g., with DocGen4)" 