# AGENTS.md for nixtamal project

## Project Overview
**Nixtamal** is a Nix version pinning tool designed as an alternative/complement to flakes. It provides sophisticated dependency management with first-class support for Darcs, Pijul, and other VCS systems that flakes don't handle well.

**Strategic Position**: While nixtamal is philosophically an alternative to flakes, toastal is implementing dual support to provide ecosystem bridge capabilities while maintaining the project's core mission.

## Build/Lint/Test Commands

### Traditional Nix Commands
- Build: `dune build`
- Test all: `dune runtest`
- Run single test: `dune runtest --filter <test_name>` (use with alcotest test names)
- Format: `dune build @fmt` (ocamlformat)
- Clean: `dune clean`

### Nix Integration Commands
- Traditional build: `nix-build` or `nix-build release.nix`
- Development shell: `nix-shell` or `nix-shell shell.nix`
- Flake build (Phase 1+): `nix flake build` or `nix build .#nixtamal`
- Flake shell (Phase 1+): `nix develop` or `nix shell .#devShell`

## Code Style Guidelines
- **Formatting**: Use ocamlformat (auto-formatted via dune @fmt)
- **Indentation**: Tabs for OCaml, spaces for Nix/dune files (per .editorconfig)
- **Naming**: Modules CamelCase, functions/values snake_case, types lowercase
- **Imports**: `open` for common modules (e.g., Name), qualified otherwise
- **Types**: Use records with `[@@deriving show, eq, make, qcheck]` for testability
- **Error Handling**: Prefer `Result` over exceptions; use `Error` module for custom errors
- **Comments**: SPDX headers required; TODOs for future work
- **Line Length**: No strict limit, but aim for readability
- **Pattern Matching**: Exhaustive, use `|` for clarity

## Phase 1: Flake Integration Planning

### Week 1 Objectives
1. **Create `flake.nix`** with core outputs wrapping existing `release.nix`
2. **Preserve existing infrastructure** as primary build method
3. **Implement basic flake outputs**:
   - `packages.${system}.default` (nixtamal package)
   - `packages.${system}.nixtamal` (explicit package name)
   - `devShells.${system}.default` (development environment)
   - `checks.${system}` (test suite integration)
   - `lib` outputs for ecosystem integration

### Week 1 Deliverables
- `flake.nix` with minimal working flake interface
- `flake.lock` generation workflow
- Documentation updates explaining dual approach
- Testing both traditional and flake workflows

### Strategic Constraints
- **Maintain philosophical consistency**: Flakes as interface, not replacement
- **Preserve existing workflows**: No breaking changes to current Nix infrastructure
- **Complement over compete**: Position as bridge between traditional Nix and flake ecosystems

## Agent Notes
- After each major change, create comprehensive notes in `./llm/` folder
- Use the template in `./llm/README.md`
- Document learnings, challenges, solutions, and insights for future reference
- **Flake integration changes** should be specifically documented with strategic reasoning

## Flake Integration Architecture

### Design Principles
1. **Wrapper Pattern**: flake.nix imports and wraps existing release.nix infrastructure
2. **Dual Interface**: Both traditional Nix and flakes remain fully functional
3. **Ecosystem Bridge**: Expose nixtamal capabilities to flake users without compromising core mission
4. **Incremental Adoption**: Users can gradually engage with flakes without abandoning nixtamal

### Key Architectural Decisions
- Keep `nix/tamal/` system as primary input management
- Use flakes only for build/packaging interface layer
- Maintain backward compatibility with existing nixtamal projects
- Provide migration utilities in future phases

No Cursor or Copilot rules found.