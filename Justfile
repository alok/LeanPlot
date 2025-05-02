# LeanPlot Justfile

# Build the project
build:
	lake build

# Watch build continuously
watch:
	lake build -w

# Lint (placeholder – adjust when linter chosen)
lint:
	lake env lean --run Std.Tactic.Lint

# Run overlay demo (opens infoview when using Lean4 editor)
# This is a noop terminal command, but included for discoverability
overlay:
	echo "Open LeanPlot/Demos/OverlayPlot.lean and put cursor on #html overlay"

# Update changelog timestamp
changelog-update:
	python - <<'PY'
from datetime import datetime, timezone
stamp = datetime.now(timezone.utc).strftime('%Y-%m-%d:%H:%M')
import pathlib, re, sys
path = pathlib.Path('CHANGELOG.md')
txt = path.read_text()
new = re.sub(r'\[0.1.0\] – [0-9-:]+','[0.1.0] – '+stamp, txt, count=1)
path.write_text(new)
print('Timestamp updated ->', stamp)
PY 