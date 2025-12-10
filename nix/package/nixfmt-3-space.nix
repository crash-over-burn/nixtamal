#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
# I don’t like *this* formatter, but it is *a* formatter. Tweaking it to expand
# to 3 spaces, helps the maker actually read the code since 2 spaces is not
# enough contrast (& Nix magic strings don’t support tabs).
{
   lib,
   writeTextFile,
   dash,
   nixfmt-rfc-style,
   stdenvNoCC,
}:

writeTextFile {
   name = "nixfmt-3-space";
   executable = true;
   destination = "/bin/nixfmt";
   text = /* sh */ ''
      #!${lib.getExe dash}
      exec ${lib.getExe nixfmt-rfc-style} --indent=3 "$@"
   '';
   checkPhase = ''
      runHook preCheck
      ${stdenvNoCC.shellDryRun} "$target"
      runHook postCheck
   '';
   meta.mainProgram = "nixfmt";
}
