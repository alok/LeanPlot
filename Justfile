# LeanPlot Development Tasks

# Default task: show available commands
default:
	@just --list

# Build the project
build:
	lake build

# Clean build artifacts
clean:
	lake clean

# Run linter
lint:
	lake env lean --run Std.Tactic.Lint

# Build and test
test: build
	lake env lean Test.lean

# Build documentation
docs:
	lake build DocsMain

# Format code (placeholder - Lean doesn't have a standard formatter yet)
format:
	@echo "No standard formatter for Lean 4 yet"

# Check for missing documentation
check-docs:
	@echo "Checking for missing documentation..."
	@rg "missing doc string" .lake/build/ || echo "No missing docs found!"

# Run all demos
demos: build
	@echo "Building all demos..."
	@find LeanPlot/Demos -name "*.lean" -exec echo "Demo: {}" \;

# Create a new demo file
new-demo NAME:
	@echo "Creating new demo: {{NAME}}"
	@touch "LeanPlot/Demos/{{NAME}}.lean"
	@echo "import LeanPlot.API" > "LeanPlot/Demos/{{NAME}}.lean"
	@echo "import LeanPlot.Components" >> "LeanPlot/Demos/{{NAME}}.lean"
	@echo "" >> "LeanPlot/Demos/{{NAME}}.lean"
	@echo "namespace LeanPlot.Demos" >> "LeanPlot/Demos/{{NAME}}.lean"
	@echo "" >> "LeanPlot/Demos/{{NAME}}.lean"
	@echo "-- Your demo code here" >> "LeanPlot/Demos/{{NAME}}.lean"
	@echo "" >> "LeanPlot/Demos/{{NAME}}.lean"
	@echo "end LeanPlot.Demos" >> "LeanPlot/Demos/{{NAME}}.lean"

# Watch for changes and rebuild
watch:
	@echo "Watching for changes..."
	@while true; do \
		fswatch -1 -r LeanPlot/ *.lean; \
		clear; \
		echo "Changes detected, rebuilding..."; \
		lake build; \
	done

# Update dependencies
update-deps:
	lake update

# Run a specific Lean file
run FILE:
	lake env lean {{FILE}}

# Generate ctags for navigation
tags:
	@echo "Generating tags..."
	@fd -e lean | xargs ctags

# Count lines of code
loc:
	@echo "Lines of Lean code:"
	@fd -e lean -x wc -l {} | sort -n

# Check for TODOs and FIXMEs
todos:
	@echo "TODOs and FIXMEs:"
	@rg -i "todo|fixme" --type lean

# Create a release
release VERSION:
	@echo "Creating release {{VERSION}}..."
	@echo "1. Update version in lakefile.toml"
	@echo "2. Update CHANGELOG.md"
	@echo "3. Commit changes"
	@echo "4. Tag release: git tag v{{VERSION}}"
	@echo "5. Push: git push && git push --tags"

# Release build (optimised)
release:
	lake build -R

# Watch build continuously
watch:
	lake build -w

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

# Run JsonKeyCheck tests
test-json-keys:
	lake exe jsonKeyCheckTest

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