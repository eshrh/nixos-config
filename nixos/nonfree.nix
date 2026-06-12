{lib, ...}: let
  inherit (lib) any getName hasPrefix;
in {
  nixpkgs.config.allowUnfreePredicate = pkg:
    any (isUnfree: isUnfree)
    (map (unfreePkg: hasPrefix unfreePkg (getName pkg)) [
      "claude-code"
      "steam"
      "steam-run"
      "steam-original"
    ]);
}
