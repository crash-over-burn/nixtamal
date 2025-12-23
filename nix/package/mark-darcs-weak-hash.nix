#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
   lib,
   writeDashBinScript,
   darcs,
   gawk,
}:

let
   path = lib.makeBinPath [
      darcs
      gawk
   ];
in
writeDashBinScript {
   name = "mark-darcs-weak-hash";
   text = /* sh */ ''
      export PATH="${path}:$PATH"

      darcs show repo \
         | awk '/^[[:space:]]*Weak Hash:/ { sub(/^[[:space:]]*Weak Hash:[[:space:]]*/, "", $0); printf $0 }' > "$PWD/_darcs/weak_hash"
   '';
}
