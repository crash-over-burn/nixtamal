# AGENTS.md for nixtamal project

## Documentation

User-facing documentation in `README.asciidoc`. Agent notes in `./llm/` (gitignore'd for security).

## Contact

Website: https://nixtamal.tech (launching soon)

Community XMPP MUC: xmpp:nixtamal@chat.toastal.in.th?join

## Build/Lint/Test Commands

### Traditional Nix
- Build: `nix-build release.nix` or `nix build`
- Test all: `nix-build release.nix -A check` or `dune runtest`
- Single test: `dune runtest --filter <test_name>` (alcotest names)
- Format: `dune build @fmt` (ocamlformat)
- Clean: `dune clean`

### Nix Flake
- Build: `nix build .#nixtamal`
- Dev shell: `nix develop`
- Coverage: `BISECT_ENABLE=YES dune runtest --instrument-with bisect_ppx --force`
- Coverage report: `bisect-ppx-report summary --coverage-path _build/default/test`

### CI Coverage
`.github/workflows/coverage.yml` runs tests with bisect_ppx, uploads reports.

## Code Style Guidelines
- Formatting: ocamlformat (`dune @fmt`)
- Indentation: Tabs for OCaml, spaces for Nix/dune (.editorconfig)
- Naming: Modules CamelCase, functions snake_case, types lowercase
- Imports: `open` common modules, qualified otherwise
- Types: Records with `[@@deriving show, eq, make, qcheck]`
- Error Handling: `Result` over exceptions; custom `Error` module
- Comments: SPDX headers; TODOs for future work
- Pattern Matching: Exhaustive with `|`

## Nix Engineering Principles
- **READ** `/speed-storage/opencode/llm/shared/` Nix files before Nix ops
- **No Home-Manager**: Pure Nix configs only
- **Declarative**: Never imperative `nix-env`, `nix-channel`
- **Tooling**: `nix-shell -p <pkg> --run <cmd>` - never install to env
- Dev shell includes checkInputs for bisect_ppx, alcotest, qcheck

## Ongoing Integrations

### Nilla Framework (Phase 2.5)
Types, codecs, manifest serialization complete. Prefetch stubbed ("not yet implemented").

**Files**:
- `lib/input.ml`: Nilla Reference/Kind
- `lib/manifest.ml`: KDL codec
- `lib/error.ml`: Nilla prefetch_method
- `lib/prefetch.ml`: Stub
- `lib/input_foreman.ml`: Display/prefetch logic
- `lib/lock_loader.ml`: Feature flag
- `lib/lockfile.ml`: Serialization
- `lib/nixtamal.ml`: meld support

### Fossil VCS (Complete)
Full support: input type, manifest/lockfile codecs, prefetch (`nix-prefetch-fossil`), lock_loader Nix gen.

### Tests/Coverage (Recent)
- `test_upgrade.ml`: Schema upgrade (backup, dry-run, validation)
- `test_fossil.ml`: Fossil codec/roundtrip
- `test_lockfile.ml`: Auto-creation, serialization
- bisect_ppx instrumentation + CI workflow

## Agent Workflow
- **Drone Selection**: @Bellana-Drone (cheap) for small changes, @TPol for adversarial verification after each task
- **Verification**: Always build (`nix build .#nixtamal`) + test (`dune runtest`) + coverage after changes
- **Phased Remediation Plan**: Phase 1 (security) complete. Phase 2: Replace failwith → Result in nixtamal.ml/input_foreman.ml
- **Commits**: Use CrashOverBurn identity. No force-push to main.

## Development State (Apr 15 2026)
- Working tree clean, master ahead origin/master by 3 commits
- Features: Fossil VCS, schema upgrade, lockfile auto-creation, bisect_ppx coverage (17 tests passing)
- Roadmap: Phase 2 (correctness), Phase 3 (Fossil/Nilla lock_loader), Phase 4 (80% coverage), Phase 5 (refactor)
- Agent notes gitignored in `./llm/`