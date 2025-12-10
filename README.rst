================================================================================
Nixtamal
================================================================================
--------------------------------------------------------------------------------
Fulfilling input pinning for Nix (& hopefully more)
--------------------------------------------------------------------------------

:author: toastal

.. role:: ac
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

• supports mirrors for fetching [1]_
• supports patch-based :ac:`VCS`\s, like Pijul & Darcs, in a first-class sense
  (tho ``nixpkgs`` will be required due to Nix ``builtins`` fetchers limitations)
• uses a :ac:`KDL` manifest file with templating instead of :ac:`CLI` input
  for inputs
• supports arbitrary commands for getting the latest change
• does not give any special privilege to any specific code forges
• source code & community will never be hosted on a proprietary,
  privacy-invasive, megacorporate platform with obligations to shareholders or
  venture capital
• licensed for your freedom
• ML-family programming (feels closer to Nix)

Future goals:

• ``nixtamal heal`` for common pitfalls in ``manifest.kdl``
• :ac:`TUI`?
• provide a flake-like specification for project layout, but with less holes


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
