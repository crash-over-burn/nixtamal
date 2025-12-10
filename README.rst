================================================================================
Nixtamal
================================================================================
--------------------------------------------------------------------------------
Fulfilling input pinning for Nix (& hopefully more)
--------------------------------------------------------------------------------

:author: toastal

.. role:: ac
.. role:: del
.. role:: t

Pronunciation
	/ˈnɪʃˌtəmˌal/ *or* /ˈnɪksˌtəˌmal/
Maker
	toastal
Put out
	2025-12
Languages
	• OCaml
	• Nix
	• :ac:`KDL`
	• :ac:`JSON`


Purpose
================================================================================

Nixtamal is a tool to pin input version like its predecessors :t:`niv`,
:t:`npins`, :t:`Pinch`, :t:`Yae`\, & so on — as well as Nix’s experimental
:t:`flakes`. Features include:

• keeps a stable reference pin to supported :ac:`VCS`\s
• supports mirrors for fetching [1]_
• supports patch-based :ac:`VCS`\s, like Pijul & Darcs, in a first-class sense
  (tho ``nixpkgs`` will be required due to Nix ``builtins`` fetchers limitations)
• uses a :ac:`KDL` manifest file with templating instead of :ac:`CLI` input
• supports arbitrary commands for getting the latest change
  for inputs
• refreshes inputs; skips frozen
• locks new sources
• supports arbitrary commands for getting the latest change which can be used
  to avoid costly downloads/clones as well as for templating
  for inputs
• does not give any special privilege to any specific code forges
• source code & community will never be hosted on a proprietary,
  privacy-invasive, megacorporate platform with obligations to shareholders or
  venture capital
• licensed for your freedom
• ML-family programming (feels closer to Nix)

Future goals:

• migrations from prior manifest × lockfile versions
• migrations from Flakes, Npins, & Niv
• more :ac:`VCS`\s
• trying to get mirror support for :ac:`VCS`\s upstreamed into Nixpkgs
• ``nixtamal heal`` for common pitfalls in ``manifest.kdl``
• :ac:`TUI`?
• provide a flake-like specification for project layout, but with less holes

.. warning::

	As this software is in the alpha stage, the maker reserves the right make
	breaking changes file schemas & :ac:`CLI` :ac:`API`. Additionally, anything after
	tagged, the maker reserves the right to obliterate & amend patches.

Quickstart
===============================================================================

Set up
-------------------------------------------------------------------------------

.. code:: console

	$ nixtamal set-up
	Creating Nixtamal directory @ ./nix/tamal
	Writing new Nixtamal EditorConfig @ ./nix/tamal/.editorconfig …
	Fetching latest value for 「nixpkgs」 …
	Prefetching input 「nixpkgs」 … (this may take a while)
	Prefetched 「nixpkgs」.
	Making manifest file @ version:0.0.1
	Writing manifest @ manifest.kdl …
	Manifest written.
	Writing lockfile @ lock.json …
	Lockfile written.
	Writing lock loader @ default.nix …
	Lock loader written.

	$ tree nix/tamal
	nix/tamal
	├── default.nix
	├── lock.json
	└── manifest.kdl

	1 directory, 3 files


Use with a Nix project — such as in a ``release.nix``
-------------------------------------------------------------------------------

.. code:: nix

	let
		inputs = import nix/tamal { };
		pkgs = import inputs.nixpkgs { };
	in
	{
		inherit (pkgs) hello;
	}

Add a new input to pin
-------------------------------------------------------------------------------

See: `<docs/manifest.rst>`_

.. code:: console

	$ nixtamal tweak

Opens text editor. & After editing …

Lock or refresh you inputs
-------------------------------------------------------------------------------

.. code:: console

	$ nixtamal lock
	$ nixtamal refresh

What next?
-------------------------------------------------------------------------------

As they say: read the manpages

.. code:: console

	$ man nixtamal
	$ man nixtamal-manifest


Building
===============================================================================

.. code:: console

	$ nix-shell -p darcs \
		--run "darcs clone 'https://darcs.toastal.in.th/nixtamal/trunk/' nixtamal"
	$ cd nixtamal
	$ nix-build


License
===============================================================================

Depending on the content, this project is licensed under one of

• :t:`GNU General Public License, version 3.0 later` (``GPL-3.0-or-later``)
• :t:`GNU Lesser General Public License version 2.1 or later`
  (``LGPL-2.1-or-later``) with & without the :t:`OCaml LGPL Linking Exception`
  (``OCaml-LGPL-linking-exception``)
• :t:`ISC License` (``ISC``)
• :t:`Creative Commons Attribution-ShareAlike 4.0 International`
  (``CC-BY-SA-4.0``)

For details read ``LICENSE.txt`` with full license texts at ``license/``.


Pitching in
================================================================================

Currently this is best done by sending a patchset to
`toastal+nixtamal@posteo.net`_ or :ac:`DM` me a remote to clone @
`toastal@toastal.in.th`_.

Community is in an :ac:`XMPP` :ac:`MUC` (chatroom) with future hopes to have an
:ac:`IRC` gateway. Join @ <xmpp:nixtamal@chat.toastal.in.th?join>.

..
	Additionally, please read the ``PITCHING_IN.rst`` file for other
	information/expectations.

.. _toastal+nixtamal@posteo.net: mailto:toastal+nixtamal@posteo.net
.. _toastal@toastal.in.th: xmpp:toastal@toastal.in.th


Funding
================================================================================

See choices at the `maker’s website <https://toast.al/funding>`_.


.. [1] :ac:`WIP` with upstream Nixpkgs

	• Darcs: https://github.com/NixOS/nixpkgs/pull/467172
	• Pijul: https://github.com/NixOS/nixpkgs/pull/467890

.. vim: set textwidth=80
