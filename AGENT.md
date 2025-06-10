# LeanPlot Development Guide

## Build Commands
- `just build` or `lake build` - Build the project
- `just test` - Run tests (`lake env lean Test.lean`)
- `just lint` - Run linter (`lake env lean --run Std.Tactic.Lint`)
- `just test-json-keys` - Run JsonKeyCheck tests (`lake exe jsonKeyCheckTest`)
- `just run FILE` - Run specific Lean file (`lake env lean FILE`)

## Code Style & Conventions
- **Imports**: Follow import hierarchy - API/Core imports first, then domain-specific modules
- **Namespaces**: Use `LeanPlot.ModuleName` structure, prefer explicit namespace declarations
- **Naming**: PascalCase for types/modules, camelCase for functions, SCREAMING_SNAKE for constants
- **Documentation**: Enable `linter.missingDocs = true` - document all public functions
- **Types**: Prefer explicit types, use `ToFloat` typeclass for numeric conversions
- **Error handling**: Use `Option` and `Except` types, avoid panics in library code

## Demo Requirements
- New plot features MUST include demo in `LeanPlot/Demos/` with `<FeatureName>Demo.lean` naming
- Update `Gallery.md` with links to new demos
- Import pattern: `LeanPlot.API` and `LeanPlot.Components` for demos

## Testing
- Simple tests go in `Test.lean`
- Complex tests in `LeanPlot/Test/` directory
- Use `#eval` for basic validation, executable tests for complex scenarios

## Visual Development Tools
- **mcp-image-extractor**: Use for visual feedback loop when developing plots - extract screenshots of rendered plots for debugging and validation
- **lean-lsp-mcp**: Provides Lean Language Server integration for better code completion and error checking
- **lean-explore**: Tool for exploring Lean code structure and definitions interactively
