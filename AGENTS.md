# AGENTS.md for nixtamal project

## Documentation

User-facing documentation is in `README.asciidoc`. Agent notes go in `./llm/` (note: this folder is now gitignored for security).

## Contact

Website: https://nixtamal.tech (launching soon)

Community XMPP MUC: xmpp:nixtamal@chat.toastal.in.th?join

## Build/Lint/Test Commands

### Traditional Nix Commands
- Build: `nix-build release.nix` or `nix build`
- Test all: `nix-build release.nix -A check` or `dune runtest`
- Run single test: `dune runtest --filter <test_name>` (use with alcotest test names)
- Format: `dune build @fmt` (ocamlformat)
- Clean: `dune clean`

### Nix Flake Commands
- Build: `nix flake build` or `nix build .#nixtamal`
- Dev shell: `nix develop` or `nix shell .#devShell`

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

## Ongoing Integrations

### Nilla Framework (In Progress)

Adding first-class support for https://github.com/nilla-nix/nilla[Nilla], a Nix framework with loaders and module system.

**Status**: Types, codecs, and manifest serialization implemented. Prefetch not yet implemented.

**Files modified**:
- `lib/input.ml` - Nilla module with Reference type, Kind variant
- `lib/manifest.ml` - KDL codec for Nilla input type
- `lib/error.ml` - Added Nilla to prefetch_method, Not_implemented error
- `lib/prefetch.ml` - Stub returning "not yet implemented"
- `lib/input_foreman.ml` - Display and needs_prefetch logic
- `lib/lock_loader.ml` - Feature constant
- `lib/lockfile.ml` - Lockfile serialization
- `lib/nixtamal.ml` - meld_input_with_lock support

### Future Integrations
- Additional VCS support as needed
- Migration utilities (flakes, npins, niv)

## Agent Notes
- After each major change, create comprehensive notes in `./llm/` folder
- Use the template in `./llm/README.md` (if present)
- Document learnings, challenges, solutions, and insights for future reference
- **Flake integration changes** should be specifically documented with strategic reasoning
