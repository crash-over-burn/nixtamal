================================================================================
Nixtamal Philosophy & Design Principles
================================================================================

.. role:: ab
.. role:: ac

------------------------------------------------------------------------------
Core Philosophy
------------------------------------------------------------------------------

Nixtamal exists because we believe:

1. **Version pinning should be VCS-agnostic**
2. **Configuration should be human-readable**
3. **Users deserve privacy and control**
4. **Software should be maintainable without megacorporate platforms**
5. **The command line is still the best interface**

------------------------------------------------------------------------------
Why KDL?
------------------------------------------------------------------------------

We chose :ac:`KDL` (KDL Document Language) over JSON, YAML, or TOML for several
reasons:

**Readability**
   KDL's node-based structure is immediately scannable. Unlike JSON's braces
   and commas, or YAML's whitespace sensitivity, KDL looks like what it
   represents:

   .. code:: kdl

      inputs {
          nixpkgs {
              git {
                  repository "https://github.com/NixOS/nixpkgs"
                  branch "nixos-unstable"
              }
          }
      }

**Semantic Richness**
   Nodes can have properties, children, and values all in one structure. This
   maps naturally to our domain model:

   .. code:: kdl

      hash algorithm=SHA256 expected="sha256-..."

**No Whitespace Ambiguity**
   Unlike YAML, KDL is unambiguous. No more "did I use tabs or spaces?"

**Comment Preservation** (Future)
   Once KDL v2 stabilizes, we can preserve comments through roundtrips,
   enabling better tooling.

------------------------------------------------------------------------------
First-Class VCS Diversity
------------------------------------------------------------------------------

**The Git Monoculture Problem**

The software industry has converged almost entirely on Git. While Git is
capable, this monoculture:

- Discourages innovation in version control
- Creates single points of failure
- Excludes projects using other VCS
- Reinforces GitHub's dominance

**Our Approach**

Nixtamal treats all VCS equally:

• **Git**: Full support with submodules, LFS, mirroring
• **Darcs**: Patch-based, context-aware locking
• **Pijul**: Channel and state-based references
• **Fossil**: Checkin-based with built-in wiki/ticket integration
• **Nilla**: Emerging framework support

Each VCS gets:
- Native prefetch integration
- Appropriate reference types (branches, tags, contexts, states)
- Proper lockfile serialization
- Documentation and examples

**Why This Matters**

When you use Darcs or Pijul, you shouldn't have to convert to Git just to use
Nix. Nixtamal meets you where you are.

------------------------------------------------------------------------------
The Anti-Corporate Stance
------------------------------------------------------------------------------

**Privacy and Surveillance**

Nixtamal will never:
- Use proprietary SaaS for core functionality
- Phone home with usage data
- Require accounts or authentication
- Track users across projects

**Platform Independence**

The source code and community are hosted on:
- Self-hosted Darcs repositories (original)
- Community-maintained Git mirrors
- XMPP for real-time communication
- nixtamal.tech for project website

We deliberately avoid:
- GitHub (Microsoft surveillance)
- Discord (proprietary, privacy-invasive)
- Slack (corporate surveillance)
- Twitter/X (disinformation platform)

**Why This Is Practical**

Modern distributed version control doesn't need a central platform. Darcs,
Pijul, and Fossil all work perfectly well without GitHub. XMPP is a mature,
federated protocol. The web existed before Cloudflare.

------------------------------------------------------------------------------
Dual Workflow Philosophy
------------------------------------------------------------------------------

**The Flake Controversy**

Nix flakes are powerful but controversial:
- Lockfiles in Git (bad for binary files)
- JSON configuration (not human-friendly)
- Centralized registry (single point of failure)
- No standardized update workflow

**Our Solution: Compatibility Without Compromise**

Nixtamal supports both workflows:

**Traditional Nix** (Default)
   - ``manifest.kdl`` in repo
   - ``lock.json`` in repo or separate
   - Works with ``nix-build``, ``nix-shell``
   - No evaluation caching issues

**Flake Integration** (Optional)
   - Use ``nixtamal.lib.fromNixtamalProject`` in flake.nix
   - Consume nixtamal projects as flake inputs
   - Generate flake.lock from nixtamal lockfile
   - Best of both worlds

**Why Both?**

Users should choose their workflow. Some teams love flakes. Others need
traditional Nix. Nixtamal doesn't force a choice.

------------------------------------------------------------------------------
Privacy by Design
------------------------------------------------------------------------------

**What We Don't Collect**

- No analytics
- No crash reporting
- No usage statistics
- No dependency telemetry

**What Stays Local**

- Lockfile generation
- Prefetch operations
- Mirror selection
- Template rendering

**Network Activity**

The only network activity is:
1. Prefetching inputs (to your own stores)
2. Checking for latest versions (when you run ``refresh``)
3. Fetching nixpkgs (if you use the default)

That's it. No metrics. No tracking. No bullshit.

------------------------------------------------------------------------------
Command Line as Primary Interface
------------------------------------------------------------------------------

**The TUI Question**

We've considered adding a Text User Interface (TUI). While appealing, we're
hesitant because:

- CLI is scriptable; TUI is not
- CLI works over SSH; TUI requires terminal support
- CLI is timeless; TUIs become dated
- CLI is accessible; TUIs may not be

**Our Compromise**

- CLI is primary and always will be
- TUI may be added as optional enhancement (Phase 4)
- All functionality available via CLI first
- Web interface for visualization (Phase 3)

------------------------------------------------------------------------------
Sustainability and Maintenance
------------------------------------------------------------------------------

**Long-term Viability**

Nixtamal is designed to be maintainable:

- OCaml is a stable, typed language with decades of history
- KDL is a simple, well-specified format
- No external service dependencies
- Clear module boundaries
- Comprehensive tests

**Community Governance**

The project follows a "benevolent dictator" model:
- toastal maintains and decides direction
- Community contributions welcome
- Forks encouraged if direction diverges
- No CLA or corporate ownership

**Funding**

Currently unfunded. If you want to support:
- Contribute code
- Write documentation
- Package for distributions
- Donate to toastal

------------------------------------------------------------------------------
Comparison with Alternatives
------------------------------------------------------------------------------

**vs. Flakes**

+ Better VCS support
+ Human-readable config (KDL vs JSON)
+ External lockfiles
+ No evaluation caching issues
- Less integrated with nix command
- Smaller ecosystem

**vs. npins**

+ First-class VCS diversity
+ KDL > JSON
+ Template support
+ Schema versioning
- More complex
- OCaml dependency

**vs. niv**

+ Modern codebase
+ Better error handling
+ Schema upgrades
- No niv import (yet)
- Less mature ecosystem

**vs. Built-in fetchers**

+ Unified interface
+ Lockfile management
+ Stale checking
+ Update workflow
- Additional dependency

------------------------------------------------------------------------------
Conclusion
------------------------------------------------------------------------------

Nixtamal represents a bet: that users want:

1. **Control** over their tooling
2. **Privacy** by default
3. **Diversity** in VCS support
4. **Compatibility** without lock-in
5. **Sustainability** over hype

If these values resonate with you, welcome. If not, there are plenty of
alternatives. Choice is the point.

.. vim: set textwidth=80
