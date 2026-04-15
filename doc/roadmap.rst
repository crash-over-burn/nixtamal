================================================================================
Nixtamal Development Roadmap
================================================================================

.. role:: ab
.. role:: ac

------------------------------------------------------------------------------
Overview
------------------------------------------------------------------------------

This roadmap outlines the planned development phases for Nixtamal, from its
initial foundation through its ultimate goal: replacing detsys SBOM and
nixpkgs dependency visualization with a superior, community-driven alternative.

**Current Status:** Phase 2.5 (Documentation & Website Preparation)

------------------------------------------------------------------------------
Phase 1: Foundation ✅ COMPLETED
------------------------------------------------------------------------------

**Timeline:** 2024-2025 (Original Development)

**Goals:** Establish core functionality and architecture

**Deliverables:**

✅ Core type system (Input, Lockfile, Manifest)
✅ KDL codec for configuration
✅ JSON codec for lockfiles
✅ Git, Darcs, Pijul VCS support
✅ File and Archive input types
✅ Concurrent prefetching with Eio
✅ Basic CLI (lock, refresh, show)
✅ Nilla framework integration (types)
✅ Test suite foundation

**Key Decisions:**
- OCaml 5.x with Eio for concurrency
- KDL for configuration format
- JSON for lockfile format
- Result-based error handling
- Property-based testing with QCheck

**Blockers Resolved:**
- Initial Darcs/Pijul fetcher upstreaming to nixpkgs
- OCaml 5/Eio ecosystem stabilization

------------------------------------------------------------------------------
Phase 2: Core Features ✅ COMPLETED
------------------------------------------------------------------------------

**Timeline:** 2026-Q1 (Completed 2026-04-14)

**Goals:** Feature parity with upstream, production readiness

**Deliverables:**

✅ **Schema Management**
   - Versioned schemas (0.1.1, 0.2.0)
   - ``nixtamal upgrade`` command
   - Backup and rollback on failure
   - Version validation and detection

✅ **Extended VCS Support**
   - Fossil VCS integration
   - Nilla prefetch implementation
   - Hash algorithm improvements (SRI support)

✅ **Build System Improvements**
   - Cmdliner 2.x compatibility
   - Flake integration enhancements
   - Cross-compilation support preparation

✅ **Quality Assurance**
   - Lockfile auto-creation
   - Comprehensive error messages
   - Test coverage infrastructure (bisect_ppx)
   - CI/CD pipeline

✅ **Developer Experience**
   - Better error context
   - Command help improvements
   - Documentation updates

**Commits:**
- ``4ed722f8`` - Port upstream patches (Cmdliner 2.x, upgrade, Fossil)

**Technical Debt:**
- Some deprecation warnings suppressed (QCheck API changes)
- Comment preservation in KDL pending KDL v2

------------------------------------------------------------------------------
Phase 2.5: Documentation & Website Preparation 🔄 CURRENT
------------------------------------------------------------------------------

**Timeline:** 2026-Q2 (Current)

**Goals:** Prepare for nixtamal.tech launch and community growth

**Deliverables:**

🔄 **Documentation**
   ✅ Comprehensive project documentation (this document set)
   - API documentation (odoc)
   - Tutorial and guide content
   - Migration guides (from flakes, npins, niv)
   - Man pages for all commands

🔄 **Website (nixtamal.tech)**
   - Static site generation
   - Documentation hosting
   - Interactive examples
   - Community links (XMPP, source)

📋 **Test Coverage**
   ✅ Coverage infrastructure (bisect_ppx)
   ✅ Basic test expansion (upgrade, Fossil, lockfile)
   🔄 Coverage target: 50%+
   🔄 Integration tests
   🔄 Property-based test expansion

📋 **Community Building**
   - Contribution guidelines
   - Issue templates
   - Security policy
   - Code of conduct

**Success Metrics:**
- Documentation complete and accurate
- Website live with all content
- Test coverage >50%
- First external contribution

------------------------------------------------------------------------------
Phase 3: Visual Dependency Graphs 📊 NEXT
------------------------------------------------------------------------------

**Timeline:** 2026-Q2/Q3 (Planned)

**Goals:** Create industry-leading dependency visualization to replace detsys
SBOM and improve upon nixpkgs visualization

**Deliverables:**

📋 **Phase 3A: Static Generation (MVP)**

   - ``nixtamal graph`` CLI command
   - SVG output for dependency trees
   - VCS type icons and indicators
   - Version information display
   - Stale dependency highlighting
   
   **Technical Stack:**
   - OCaml: ocamlgraph for graph processing
   - Layout: Sugiyama or force-directed
   - Output: SVG (primary), PNG (secondary)

📋 **Phase 3B: Interactive Web Component**

   - JavaScript visualization library (D3.js or Cytoscape.js)
   - Expandable/collapsible nodes
   - Filter by VCS type, update status
   - Zoom and pan
   - Search functionality
   - Export options (SVG, PNG, JSON)
   
   **Integration:**
   - Embed in nixtamal.tech
   - CI/CD badge generation
   - GitHub/GitLab integration (optional)

📋 **Phase 3C: Advanced Features**

   - Dependency diff between versions
   - License information display
   - Security advisory integration
   - Dependency health scoring
   - Reverse dependency lookup

**Competitive Advantages:**

vs. **detsys SBOM**:
- Shows relationships, not just inventory
- Real-time updates (stale checking)
- VCS diversity visualization
- Native Nix integration

vs. **nixpkgs visualization**:
- Modern, interactive interface
- Better VCS support display
- Stale/frozen indicators
- Per-project graphs (not just nixpkgs)

**Design Document:**
See ``llm/2026-04-15-dependency-graph-design.md`` for detailed technical
specification.

**Success Metrics:**
- Graph generation <1s for projects with <100 deps
- Web component loads <3s
- Featured on nixtamal.tech homepage
- Community adoption for SBOM replacement

------------------------------------------------------------------------------
Phase 4: Ecosystem & Advanced Features 🔮 FUTURE
------------------------------------------------------------------------------

**Timeline:** 2026-Q4+ (Future)

**Goals:** Mature ecosystem, advanced workflows, optional TUI

**Potential Deliverables:**

💡 **TUI (Text User Interface)**
   - Optional ncurses-based interface
   - Interactive lock/refresh
   - Real-time progress display
   - Keep CLI as primary interface

💡 **Migration Tools**
   - Import from flakes
   - Import from npins
   - Import from niv
   - Automatic conversion utilities

💡 **Advanced Lockfile Features**
   - Partial updates (update only specific inputs)
   - Lockfile merging (monorepo support)
   - Lockfile verification/signing
   - Distributed lockfile caching

💡 **Nix Integration**
   - NixOS module for system-wide nixtamal
   - Home Manager integration (optional, we prefer pure Nix)
   - nix-shell integration
   - direnv integration

💡 **Performance & Scale**
   - Incremental locking
   - Parallel prefetch optimization
   - Large monorepo support (>1000 inputs)
   - Memory usage optimization

💡 **Enterprise Features** (if there's demand)
   - Private mirror management
   - Policy enforcement
   - Audit logging
   - Team workflows

------------------------------------------------------------------------------
Continuous Improvement
------------------------------------------------------------------------------

**Ongoing Throughout All Phases:**

• **Bug fixes** - Priority 0
• **Security updates** - Priority 0
• **Documentation updates** - Keep current
• **Performance optimization** - Regular profiling
• **Community support** - Responsive issues/PRs
• **Upstream tracking** - Monitor nixpkgs, OCaml ecosystem

**Dependency Updates:**

• Regular OCaml version updates
• Eio ecosystem updates
• KDL library updates (especially v2 when stable)
• Nix ecosystem compatibility

------------------------------------------------------------------------------
Release Cadence
------------------------------------------------------------------------------

**Versioning:** Semantic Versioning (SemVer)

**Release Types:**

**Patch releases (0.x.Y)**
   - Bug fixes
   - Security patches
   - Documentation fixes
   - Frequency: As needed

**Minor releases (0.X.y)**
   - New features
   - Non-breaking changes
   - New VCS support
   - Frequency: Monthly during active development

**Major releases (X.y.z)**
   - Breaking changes
   - Schema updates (with upgrade path)
   - API changes
   - Frequency: When necessary, with migration period

**Current Version:** 0.2.0 (Schema Version)

**Next Planned:**
- 0.2.1 - Documentation polish
- 0.3.0 - Graph visualization (Phase 3A)
- 1.0.0 - Stable release (after Phase 4 begins)

------------------------------------------------------------------------------
Decision Log
------------------------------------------------------------------------------

**2024:** Decision to use KDL over JSON/YAML/TOML  
*Rationale: Human-readable, semantic richness, no whitespace ambiguity*

**2024:** Decision to use OCaml over Rust/Go  
*Rationale: Type safety, ecosystem familiarity, maintainability*

**2025:** Decision to support dual workflow (traditional + flakes)  
*Rationale: User choice, compatibility, gradual migration*

**2026:** Decision to port from Darcs to Git  
*Rationale: Community accessibility, tooling ecosystem*

**2026:** Decision to prioritize graph visualization over TUI  
*Rationale: Web accessibility, SBOM replacement goal, broader impact*

**2026:** Decision to self-host vs GitHub  
*Rationale: Privacy, independence, philosophical alignment*

------------------------------------------------------------------------------
Contributing to the Roadmap
------------------------------------------------------------------------------

**How to Influence:**

1. **Open an issue** describing your use case
2. **Propose a PR** implementing a feature
3. **Discuss in XMPP** real-time chat
4. **Sponsor development** if you need specific features

**What's Needed:**

- Phase 2.5: Technical writers, web developers
- Phase 3: Visualization experts, D3.js developers
- Phase 4: Nix ecosystem experts
- Always: OCaml developers, testers, documentation

**Roadmap Evolution:**

This roadmap is a living document. As we learn from usage and community
feedback, priorities may shift. The goal remains constant: a better way to
manage Nix dependencies.

.. vim: set textwidth=80
