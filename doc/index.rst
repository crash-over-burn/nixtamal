================================================================================
Nixtamal Project Documentation
================================================================================

.. role:: ab
.. role:: ac

A Nix version pinning tool with first-class support for diverse version control
systems and modern dependency management.

------------------------------------------------------------------------------
Overview
------------------------------------------------------------------------------

Nixtamal is an OCaml-based tool that addresses the limitations of Nix flakes
while maintaining compatibility with traditional Nix workflows. It provides
deterministic dependency pinning through lockfiles while supporting a diverse
ecosystem of version control systems.

**Key Capabilities:**

• Multi-VCS support: Git, Darcs, Pijul, Fossil, and archives/files
• KDL-based declarative configuration
• Concurrent locking with domain-based parallelism
• Schema versioning with upgrade paths
• Template/Jinja2 support for dynamic URLs
• Mirror support for resilience
• Dual workflow: Traditional Nix + Flake compatibility

**Website:** https://nixtamal.tech  
**Community:** xmpp:nixtamal@chat.toastal.in.th?join

------------------------------------------------------------------------------
Quick Links
------------------------------------------------------------------------------

.. toctree::
   :maxdepth: 2

   philosophy
   manifest
   roadmap
   history

------------------------------------------------------------------------------
Current State (Phase 2)
------------------------------------------------------------------------------

**Version:** 0.2.0 (Schema Version)

**Implemented Features:**

✅ **Core Functionality**
   - Manifest parsing and validation (KDL)
   - Lockfile generation and management (JSON)
   - Concurrent prefetching with Eio
   - All major VCS types supported

✅ **Schema Management**
   - Versioned schemas (0.1.1 → 0.2.0)
   - ``nixtamal upgrade`` command
   - Automatic backup and rollback

✅ **VCS Support**
   - Git (with submodules, LFS)
   - Darcs (with context/tag references)
   - Pijul (channels, states, changes)
   - Fossil (branches, tags, checkins)
   - Nilla framework integration
   - Files and archives

✅ **Build System**
   - Traditional Nix (default.nix, shell.nix)
   - Flake support (flake.nix)
   - Hybrid workflows

✅ **Quality Assurance**
   - Comprehensive test suite with coverage (36.87%)
   - Property-based testing (QCheck)
   - CI/CD integration

**In Development:**

🔄 **Phase 2.5** (Current Focus)
   - Documentation website (nixtamal.tech)
   - Test coverage expansion
   - Visual dependency graphs

------------------------------------------------------------------------------
Installation
------------------------------------------------------------------------------

**Via Nix Flake:**

.. code:: bash

   nix run github:toastal/nixtamal -- --help

**Via Traditional Nix:**

.. code:: bash

   nix-build release.nix
   ./result/bin/nixtamal --help

**Development Shell:**

.. code:: bash

   nix develop
   dune build
   dune runtest

------------------------------------------------------------------------------
Basic Usage
------------------------------------------------------------------------------

**Initialize a project:**

.. code:: bash

   nixtamal set-up

**Lock dependencies:**

.. code:: bash

   nixtamal lock

**Refresh to latest versions:**

.. code:: bash

   nixtamal refresh

**Upgrade schema:**

.. code:: bash

   nixtamal upgrade --dry-run  # Preview changes
   nixtamal upgrade            # Apply upgrade

**Check for stale dependencies:**

.. code:: bash

   nixtamal list-stale

------------------------------------------------------------------------------
Architecture
------------------------------------------------------------------------------

**Core Modules:**

``lib/input.ml``
   Input type definitions for all supported VCS and file types

``lib/manifest.ml``
   KDL parsing and validation for manifest.kdl

``lib/lockfile.ml``
   Lockfile serialization and deserialization

``lib/prefetch.ml``
   VCS-specific prefetch implementations

``lib/nixtamal.ml``
   Core orchestration and workflow logic

``lib/schema.ml``
   Schema versioning and upgrade logic

``lib/input_foreman.ml``
   Concurrent input management and locking

``bin/cmd.ml``
   CLI command definitions and interface

------------------------------------------------------------------------------
Philosophy & Design Principles
------------------------------------------------------------------------------

See :doc:`philosophy` for detailed discussion of:

• Why KDL over JSON/YAML
• First-class VCS diversity
• The anti-corporate stance
• Privacy and decentralization
• Dual workflow philosophy

------------------------------------------------------------------------------
Development Roadmap
------------------------------------------------------------------------------

See :doc:`roadmap` for complete phased development plan:

• **Phase 1** ✅ Foundation (Completed)
• **Phase 2** ✅ Core Features (Completed) 
• **Phase 2.5** 🔄 Website & Documentation (Current)
• **Phase 3** 📊 Visual Dependency Graphs (Next)
• **Phase 4** 🔮 Future Enhancements

------------------------------------------------------------------------------
Development History
------------------------------------------------------------------------------

See :doc:`history` for complete project timeline:

• Origins as a community fork
• Transition from Darcs to Git
• Flake integration development
• Nilla framework support
• Upstream porting effort (2026-04-14)

------------------------------------------------------------------------------
Contributing
------------------------------------------------------------------------------

**Code Style:**
- Tabs for OCaml, spaces for Nix/dune (per .editorconfig)
- OCamlFormat for formatting
- SPDX headers required
- Prefer Result over exceptions

**Communication:**
- Website: https://nixtamal.tech
- XMPP MUC: xmpp:nixtamal@chat.toastal.in.th?join

**License:**
GPL-3.0-or-later (tools) / LGPL-2.1-or-later (libraries)

------------------------------------------------------------------------------
.. vim: set textwidth=80
