================================================================================
Nixtamal Manifest
================================================================================
--------------------------------------------------------------------------------
Setting up for pinning down inputs
--------------------------------------------------------------------------------

:author: toastal
:version: 0.1.0

.. role:: ab
.. role:: ac
.. role:: t

Synopsis
================================================================================

Nixtamal uses :ac:`KDL` for its manifest describing inputs. At the highest
level, this includes:

• manifest version
• default hashing algorithm
• list of inputs

  	• the input kind & its specific attributes
  	• a command to check if ‘latest’
  	• hashing information

.. note::

   While :ac:`KDL` Schema Language exists, it seems there is a upcoming `v2
   <https://github.com/kdl-org/kdl/pull/486>`_. It seems to address some issues
   with the v1 :ab:`spec` which would both help, but also render using the
   existing :ab:`spec` obsolete. With this note, I will be hand-wavingly
   explaining the spec here instead of providing an actual schema.

Default ``manifest.kdl``
================================================================================

.. code:: text

	version "0.1.0"
	inputs {
		nixpkgs {
			archive {
				url "https://github.com/NixOS/nixpkgs/archive/{{cmd_value}}.tar.gz"
			}
			hash algorithm=SHA256
			latest-cmd {
				$ git ls-remote --heads "https://github.com/NixOS/nixpkgs.git" --refs "refs/heads/nixos-unstable"
				| cut -f1
			}
		}
	}


Top-level nodes
================================================================================

``version``
	Version of the Nixtamal spec the ``manifest.kdl`` is using.
``default_hash_algorithm``
	Hash algorithm to use by default for inputs when the input does not note its
	hash algorithm. Defaults to ``SHA256``.
``inputs``
	Map of inputs to be pinned where the input+node name should be unique & will
	be used in the Nix output as well as logs & errors. These nodes also can
	have a ``frozen`` property to be skipped during refreshes. See `Input
	node`_.


Input node
================================================================================

At a high level these should be seen as

“kind”
	There are specific nodes for each different type of supported
	fetchers/prefetchers: ``file``, ``archive``, ``git``, ``darcs``, ``pijul``
	(with more to come in the future).
``hash``
	An optional node for hash algorithm information for a input. The
	``algorithm`` property will be used when prefetching, locking, & for
	importing (which falls back to top-level ``default_hash_algorithm`` or
	defined default ``SHA256``). The optional ``expected`` property may be used
	to assert a known hash.

	.. caution ::

		The bootstrapping Nixpkgs pin (either manually set or using
		``nixpkgs-nixtamal`` or ``nixpkgs`` as defaults) *must* be SHA256 to be
		compatible with ``builtins.fetchTarball``.
``latest-cmd``
	Command (with or without pipes using ``$`` & ``|`` nodes) that can shelled
	out to to return a string that will be locked as the latest command value
	which can be used both to prevent unnecessary prefectching, but also for use
	in a `Templated node`_.

File
--------------------------------------------------------------------------------

``url``
	`Templated node`_ :ac:`URL` reference for the input
``mirrors``
	`Templated node`_ :ac:`URL` mirror references for the input

Archive
--------------------------------------------------------------------------------

``url``
	`Templated node`_ :ac:`URL` reference for the input
``mirrors``
	`Templated node`_ :ac:`URL` mirror references for the input

Git
--------------------------------------------------------------------------------

``repository``
	`Templated node`_ repository reference for the input
``mirrors``
	`Templated node`_ repository mirror references for the input

	.. warning::

		Probably not yet supported upstream.
“reference”
	``branch`` *or* ``ref`` node as the reference point for getting stable
	reference
``submodules``
	Leaf node for enabling submodules on a repository
``lfs``
	Leaf node for enabling Git :ac:`LFS` on a repository

Darcs
--------------------------------------------------------------------------------

``repository``
	`Templated node`_ repository reference for the input
``mirrors``
	`Templated node`_ repository mirror references for the input

	.. note::

		Recently upstreamed. See: https://github.com/NixOS/nixpkgs/pull/467172
“reference”
	``context`` *or* ``tag`` node as the reference point for getting stable
	reference; in the case of Darcs, if neither is supplied a ``context`` will
	be assumed & copied from ``nix-prefetch-darcs``

Pijul
--------------------------------------------------------------------------------

``remote``
	`Templated node`_ remote reference for the input
``mirrors``
	`Templated node`_ remote mirror references for the input

	.. note::

		Recently upstreamed. See: https://github.com/NixOS/nixpkgs/pull/467890
“reference”
	``channel`` *or* ``state`` *or* ``change`` (not recommended) node as the
	reference point for getting stable reference; if unsure, try ``channel
	main``


Templated node
================================================================================

Some nodes have values with string substitution via `Jingoo
<https://tategakibunko.github.io/jingoo/templates/templates.en.html>`_, which
is probably overkill, but could give you flexibilty with ``if`` statements. The
templated nodes include:

• ``inputs >> file > url``
• ``inputs >> file > mirrors``
• ``inputs >> archive > url``
• ``inputs >> archive > mirrors``
• ``inputs >> git > repository``
• ``inputs >> git > mirrors``
• ``inputs >> darcs > repository``
• ``inputs >> darcs > mirrors``
• ``inputs >> pijul > remote``
• ``inputs >> pijul > mirrors``
• ``inputs >> latest-cmd > $``
• ``inputs >> latest-cmd > |``

The input kind affects the values for substition:

.. csv-table:: ``file``
	:header: Key, Type, Description

	``name``, string, input name
	``cmd_value``, string nullable, latest command return value

.. csv-table:: ``archive``
	:header: Key, Type, Description

	``name``, string, input name
	``cmd_value``, string nullable, latest command return value

.. csv-table:: ``git``
	:header: Key, Type, Description

	``name``, string, input name
	``cmd_value``, string nullable, latest command return value
	``branch``, string nullable, branch name
	``ref``, string nullable, reference name
	``datetime``, string nullable, Datetime of latest revision
	``lfs``, bool, repository uses LFS
	``submodules``, bool, repository uses submodules
	``rev`` / ``revision``, string nullable, latest revision

.. csv-table:: ``darcs``
	:header: Key, Type, Description

	``name``, string, input name
	``cmd_value``, string nullable, latest command return value
	``context``, string nullable, path to context file
	``tag``, string nullable, tag
	``datetime``, string nullable, datetime of latest patch
	``weak_hash``, string nullable, latest weak hash of the repository

.. csv-table:: ``pijul``
	:header: Key, Type, Description

	``name``, string, input name
	``cmd_value``, string nullable, latest command return value
	``channel``, string nullable, remote channel
	``change``, string nullable, change
	``datetime``, string nullable, datetime of latest patch
	``state``, string nullable, latest state of the remote or supplied state


Input showcase
================================================================================

Darcs using exposed WeakHash to avoid needless refresh
--------------------------------------------------------------------------------

.. code:: text

	nixtamal {
		darcs {
			repository "https://darcs.toastal.in.th/nixtamal/trunk"
		}
		latest-cmd {
			$ curl -sL "https://darcs.toastal.in.th/nixtamal/trunk/_darcs/weak_hash"
		}
	}

File with mirror + templated nodes
--------------------------------------------------------------------------------

.. code:: text

	mozilla-tls-guidelines {
		file {
			url "https://ssl-config.mozilla.org/guidelines/{{cmd_value}}.json"
			mirrors "https://raw.githubusercontent.com/mozilla/ssl-config-generator/refs/tags/v{{cmd_value}}/src/static/guidelines/{{cmd_value}}.json"
		}
		latest-cmd {
			$ curl -sL "https://wiki.mozilla.org/Security/Server_Side_TLS"
			| htmlq -w -t "table.wikitable:last-of-type > tbody > tr:nth-child(2) > td:first-child"
			| head -n1
		}
	}

Basic Pijul with BLAKE3 hash
--------------------------------------------------------------------------------

.. code:: text

	pijul {
		pijul {
			remote "https://nest.pijul.com/pijul/pijul"
			channel main
		}
		hash algorithm=BLAKE3
	}

.. vim: set textwidth=80
