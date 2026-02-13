================================================================================
Changelog
================================================================================

.. default-role:: code

.. meta::
   :keywords: nixtamal, changelog
   :description: Changes made to Nixtamal

1.0.0 (2026-02-13)
--------------------------------------------------------------------------------

• 1.0.0’d the schema
• Add `nixtamal upgrade` to upgrade schema versions
• Add Fossil support
• Fix some bugs in how the :ac:`TUI` was handled

0.3.1-beta (2026-02-07)
--------------------------------------------------------------------------------

• Fix refactor oversight where locking wasn’t getting the fresh command/value

0.3.0-beta (2026-02-07)
--------------------------------------------------------------------------------

• Add :ac:`TUI`
• Fix missing lockfile loader semicolon on no local patches

0.2.1-beta (2026-02-03)
--------------------------------------------------------------------------------

• Add `fetch-time` to `nixtamal show`

0.2.0-beta (2026-02-02)
--------------------------------------------------------------------------------

• Update schema from 0.4.0 → 0.5.0 (will require manual migration
  [beta_man_migr]_)

  • New feature: fetch time
  • Git: now uses the ref in the lockfile to properly get the rev when fetching
  • Git: supports tags (convenience, previously used `rev` overloaded like Nix)
  • Loader: renaming for clarity `nixpkgs` → `bootstrap-nixpkgs`

• Fetch at either eval or build time (`builtins.fetch*` :ab:`vs.` `pkgs.fetch*`)
• Fix typo in fetch-git block which prevented Git usage
• Fix nix builds to work on Darwin (thanks @WeeHat)
• Remove that Nix code to remove stray OCaml references (as it doesn’t work as
  is does in other OCaml projects, & upstream Nix OCaml builds need to be looked
  at)

0.1.1-beta (2026-01-27)
--------------------------------------------------------------------------------

• Add CHANGELOG.rst
• Add {CHANGELOG,README}.rst to Nixtamal’s source filter
• Move nixtamal-manifest.schema → nitxtamal/schema/manifest.kdl
• Tweak Nix code to wrap the binary with the `$PATH` for prefetchers
• Tweak Nix code to remove stray OCaml references

.. [beta_man_migr] 1. `rm ${NIXTAMAL_DIRECTORY:-nix/tamal}/{lock.json,default.nix}`
   2. `nitamal tweak` to bump version in `manifest.kdl`
   3. re-run `nixtamal lock` to generate a new lockfile

.. vim: set textwidth=80
