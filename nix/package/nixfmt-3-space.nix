#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
   lib,
   writeDashBinScript,
   nixfmt-rfc-style,
}:

writeDashBinScript {
   name = "nixfmt";
   text = /* sh */ ''
      exec ${lib.getExe nixfmt-rfc-style} --indent="3" "$@"
   '';
   meta = {
      description = "nixfmt, but with 3-space indentation";
      longDescription = ''
         I don’t like *this* formatter, but it is *a* formatter. Tweaking it to
         expand to 3 spaces, helps the maker actually read the code since 2
         spaces is not enough contrast (& Nix magic strings don’t support tabs).
      '';
   };
}
