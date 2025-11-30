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
	lake lint

# Generate documentation images (SVG)
doc-images:
	lake exe gendocimages

# Build Verso documentation
docs: doc-images
	lake build leanplot-docs
	.lake/build/bin/leanplot-docs
	@echo "Documentation generated in _out/docs/"
	@echo "Serve with: python3 -m http.server 8000 --directory _out/docs/html-multi"

# Serve documentation locally
docs-serve: docs
	python3 -m http.server 8000 --directory _out/docs/html-multi

# Check for missing documentation
check-docs:
	@echo "Checking for missing documentation..."
	@rg "missing doc string" .lake/build/ || echo "No missing docs found!"

# List all demos
demos:
	@echo "Available demos:"
	@find LeanPlot/Demos -name "*.lean" -exec basename {} .lean \; | sort

# Build the PNG export example
png-demo:
	lake -d examples/png-export build

# Create a new demo file
new-demo NAME:
	@echo "Creating new demo: {{NAME}}"
	@cat > "LeanPlot/Demos/{{NAME}}.lean" << 'EOF'
	import LeanPlot.API
	import LeanPlot.DSL

	open LeanPlot.API

	namespace LeanPlot.Demos.{{NAME}}

	-- Your demo code here
	#plot (fun x => x^2)

	end LeanPlot.Demos.{{NAME}}
	EOF

# Watch for changes and rebuild
watch:
	lake build -w

# Update dependencies
update-deps:
	lake update

# Run a specific Lean file
run FILE:
	lake env lean {{FILE}}

# Count lines of code
loc:
	@echo "Lines of Lean code:"
	@fd -e lean -E .lake | xargs wc -l | tail -1

# Check for TODOs and FIXMEs
todos:
	@echo "TODOs and FIXMEs:"
	@rg -i "todo|fixme" --type lean -g '!.lake' || echo "None found!"

# Release build (optimised)
release-build:
	lake build -R

# Lean REPL
repl:
	lake repl

# Install git hooks
hooks-install:
	git config core.hooksPath .githooks
	chmod +x .githooks/*
	@echo "git hooks path set to .githooks"

# Run JsonKeyCheck tests
test-json-keys:
	lake exe jsonKeyCheckTest

# Clean and rebuild completely
rebuild: clean build
	@echo "Clean rebuild completed"

# Show project statistics
stats:
	@echo "=== Project Statistics ==="
	@echo "Core modules:"
	@find LeanPlot -maxdepth 1 -name "*.lean" | wc -l
	@echo "Demo files:"
	@find LeanPlot/Demos -name "*.lean" | wc -l
	@echo "Total lines of Lean code:"
	@fd -e lean -E .lake | xargs cat | wc -l

# Export sampled data as JSON
export-json fn out='out.json' steps='200' min='0.0' max='1.0':
	lake build leanplot-export
	.lake/build/bin/leanplot-export --fn {{fn}} --out {{out}} --steps {{steps}} --min {{min}} --max {{max}}
