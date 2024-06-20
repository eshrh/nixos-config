{lib, ...}:
let
  inherit (lib) any getName hasPrefix;
in
{
  nixpkgs.config.allowUnfreePredicate = pkg:
    any (isUnfree: isUnfree)
      (map (unfreePkg: hasPrefix unfreePkg (getName pkg)) [
        "osu-lazer-bin"
        "zoom"

        "steam"
        "steam-run"
        "steam-original"
      ]);
}
