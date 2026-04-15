================================================================================
Nixtamal Development History
================================================================================

.. role:: ab
.. role:: ac

------------------------------------------------------------------------------
Origins (2024)
------------------------------------------------------------------------------

**The Problem**

In 2024, toastal was working with Nix in environments that required diverse
version control systems. The Nix ecosystem at the time was heavily Git-centric:

- Flakes only supported Git repositories well
- Darcs and Pijul projects required manual workarounds
- No standardized way to pin non-Git dependencies
- Existing tools (niv, npins) had limited VCS support

**The First Spark**

The initial idea was simple: a version pinning tool that treats all VCS equally.
Not Git-first with other VCS as afterthoughts, but truly first-class support
for the diversity of version control systems.

**Technology Choice**

OCaml was chosen for several reasons:
- Strong type system for complex domain modeling
- Excellent concurrency support (Eio library)
- Decades of stability and maintainability
- Personal familiarity
- Nix integration already well-established

KDL was chosen for configuration because:
- More readable than JSON
- More structured than TOML
- No YAML whitespace hell
- Native comment support (important for hand-edited files)

------------------------------------------------------------------------------
Early Development (2024-2025)
------------------------------------------------------------------------------

**Darcs-First Approach**

The project was initially developed in Darcs, hosted at:
``https://darcs.toastal.in.th/nixtamal/trunk``

This was a deliberate choice:
- Dogfooding the VCS diversity principle
- Darcs' patch theory is elegant
- Self-hosting independence

**Version 0.1.x Series**

The early versions focused on establishing the core:

- 0.1.0 - Basic manifest/lockfile structure
- 0.1.1 - Stabilized schema with proper versioning
- Git, Darcs, and Pijul support added iteratively
- File and Archive inputs for completeness

**Key Technical Decisions:**

1. **Concurrent prefetching** using OCaml 5's Eio library
2. **Template system** using Jingoo for dynamic URLs
3. **KDL codecs** for type-safe configuration parsing
4. **Lockfile format** as JSON (machine-readable, diffable)

------------------------------------------------------------------------------
The Nilla Integration (2025)
------------------------------------------------------------------------------

**What is Nilla?**

Nilla (https://github.com/nilla-nix/nilla) is a Nix framework with loaders
and a module system. It represents a new approach to Nix project structure.

**Integration Work**

Adding Nilla support required:
- New input type for Nilla references
- Understanding Nilla's loader architecture
- Manifest serialization for Nilla inputs
- Lockfile integration

**Status:** Types, codecs, and manifest serialization implemented.
Prefetch support deferred pending Nilla ecosystem maturation.

**Impact:** Nixtamal can now manage Nilla-based inputs alongside traditional
Nix dependencies, supporting hybrid workflows.

------------------------------------------------------------------------------
The Git Migration (2025-2026)
------------------------------------------------------------------------------

**Why Migrate?**

Despite philosophical alignment with Darcs, practical realities emerged:

1. **Contributor friction** - Most developers know Git, not Darcs
2. **Tooling gaps** - CI/CD, GitHub Actions, etc. built around Git
3. **Visibility** - Git repositories get more community attention
4. **Accessibility** - Easier for contributors to submit patches

**The Fork**

A community-maintained Git fork was created. The original Darcs repository
continues to exist as the "source of truth" with the Git repository as a
mirror with additional development.

**Maintaining Principles:**

The migration didn't compromise core principles:
- Still self-hosted (not GitHub)
- XMPP remains primary communication
- Website still independent
- Philosophy document unchanged

------------------------------------------------------------------------------
The Flake Integration (2025-2026)
------------------------------------------------------------------------------

**The Controversy**

Nix flakes are controversial in the community:
- Pro: Integrated locking, caching, composability
- Con: JSON configuration, Git lockfiles, centralized registry

**The Compromise**

Rather than choosing sides, Nixtamal embraced both:

**Traditional Nix** (Default)
- KDL configuration (human-readable)
- External lockfiles
- Works with ``nix-build``, ``nix-shell``

**Flake Integration** (Optional)
- ``nixtamal.lib`` for flake consumption
- Hybrid workflows supported
- Use nixtamal projects as flake inputs

**Technical Implementation:**

- ``flake.nix`` with proper overlay system
- ``nix/tamal/`` directory for lockfile consumption
- Library functions for ecosystem integration
- Phase 1 implementation notes documented

**Outcome:** Users choose their workflow. No forced migration.

------------------------------------------------------------------------------
Major Upstream Port (2026-04-14)
------------------------------------------------------------------------------

**The Gap**

The Git fork had diverged from upstream Darcs:
- Git fork: 145 commits, ~0.3.x-alpha
- Darcs upstream: 347 patches, v1.1.2
- ~202 patches behind
- Missing: schema upgrade, Fossil VCS, TUI, etc.

**The Port**

A systematic porting effort was undertaken:

**Phase 1 - Critical Fixes:**
- Cmdliner 2.x compatibility (variable shadowing fixes)
- Lockfile auto-creation (don't error on missing lockfile)

**Phase 2 - Major Features:**
- Schema upgrade command with backup/rollback
- Fossil VCS support (complete implementation)
- Hash SRI parsing improvements
- Error type refinements

**Challenges:**

1. **Type compatibility** - Error types evolved between versions
2. **Module renames** - Foreman vs input_foreman
3. **Pattern match completeness** - Fossil added to all pattern matches
4. **Flake preservation** - Must maintain existing flake integration

**Resolution:**

Bellana-Sugar and Bellana-Drone collaborated to resolve type issues while
preserving the Git fork's flake additions. All tests passing.

**Commit:**
``4ed722f8be5a3bb5ea8ffdfe89f87b75924ca030``

**Author:** CrashOverBurn <151276996+crash-over-burn@users.noreply.github.com>

------------------------------------------------------------------------------
Documentation & Website Era (2026-Q2) - CURRENT
------------------------------------------------------------------------------

**The Realization**

Code without documentation is incomplete. As nixtamal.tech prepares to launch,
comprehensive documentation became essential:

**Documentation Sprint:**

1. **Project overview** (doc/index.rst)
   - Current state summary
   - Quick links
   - Architecture overview

2. **Philosophy document** (doc/philosophy.rst)
   - Why KDL?
   - VCS diversity rationale
   - Anti-corporate stance
   - Privacy principles

3. **Roadmap** (doc/roadmap.rst)
   - Phased development plan
   - Phase 3: Visual dependency graphs (next)
   - Phase 4: Future enhancements

4. **History** (doc/history.rst) - this document
   - Project origins
   - Key decisions
   - Migration stories

**Test Coverage:**

- bisect_ppx integration
- Coverage reporting in CI
- Target: 50%+ coverage
- New tests: upgrade, Fossil, lockfile

**Website Preparation:**

- nixtamal.tech domain secured
- Static site generation planned
- Interactive examples prepared
- Community links established

------------------------------------------------------------------------------
Key Decisions & Their Impact
------------------------------------------------------------------------------

**2024: OCaml over Rust/Go**

*Decision:* Use OCaml despite Rust's popularity
*Impact:* Excellent type safety, maintainable codebase, slower initial development
*Status:* ✅ Validated - codebase stable and maintainable

**2024: KDL over JSON/YAML**

*Decision:* KDL for configuration format
*Impact:* Positive user feedback on readability, some tooling gaps
*Status:* ✅ Validated - users appreciate the format

**2025: Self-hosting over GitHub**

*Decision:* Avoid GitHub/Microsoft platforms
*Impact:* Reduced visibility, maintained independence
*Status:* ✅ Validated - aligns with philosophy

**2025-2026: Dual workflow (traditional + flakes)**

*Decision:* Support both Nix workflows
*Impact:* Broader adoption, more complex codebase
*Status:* ✅ Validated - users appreciate choice

**2026: Prioritize graphs over TUI**

*Decision:* Visual dependency graphs before TUI
*Impact:* Web-first approach, SBOM replacement goal
*Status:* 🔄 In progress - Phase 3

------------------------------------------------------------------------------
Community & Contributions
------------------------------------------------------------------------------

**Current State:**

- Primary: toastal (author)
- XMPP chat: xmpp:nixtamal@chat.toastal.in.th?join
- Website: https://nixtamal.tech (launching)
- Source: Self-hosted Git repository

**Contribution History:**

While primarily a solo project, contributions have come from:
- Subagent-assisted development (Bellana-Drone, TPol, etc.)
- Upstream patches (toastal on Darcs)
- OCaml ecosystem improvements (indirect)

**Future:**

As nixtamal.tech launches, community growth is expected:
- Clear contribution guidelines (Phase 2.5)
- Issue templates
- Beginner-friendly tasks
- Sponsorship opportunities

------------------------------------------------------------------------------
Lessons Learned
------------------------------------------------------------------------------

**Technical:**

1. **Type systems are worth it** - OCaml's strict typing caught many bugs
2. **Concurrency is hard** - Eio helped, but still complex
3. **Format stability matters** - Schema versioning was the right call
4. **Test early, test often** - Coverage infrastructure pays off

**Social:**

1. **Principles have costs** - Self-hosting reduces visibility
2. **Tooling friction is real** - Darcs → Git migration validated
3. **Documentation is code** - As important as functionality
4. **Community takes time** - Sustainable pace over hype

**Project Management:**

1. **Phased development works** - Clear milestones help focus
2. **Upstream tracking is essential** - Port before divergence grows
3. **Automation saves time** - CI/CD, coverage, docs generation
4. **Defer complexity** - TUI, patch management can wait

------------------------------------------------------------------------------
The Future
------------------------------------------------------------------------------

From here, Nixtamal aims to:

1. **Launch nixtamal.tech** with comprehensive documentation
2. **Build visual dependency graphs** (Phase 3) to replace detsys SBOM
3. **Grow community** through clear contribution paths
4. **Maintain principles** while improving accessibility
5. **Become the default** for Nix dependency management

The journey from a personal tool to a community project continues. The
foundation is solid. The vision is clear. The work continues.

------------------------------------------------------------------------------
.. vim: set textwidth=80
