{
  config,
  pkgs,
  lib,
  ...
}: let
  wal =
    (pkgs.runCommandLocal "wallpaper-pywal" {
      nativeBuildInputs = [pkgs.pywal];
    }) ''
      cp ${./wallpaper.png} wallpaper.png
      HOME="." wal -i wallpaper.png -n
      install -Dm755 .cache/wal/sequences -T $out
    '';
in {
  home.file.".cache/wal/sequences".source = "${wal}";
  home.file.".background-image".source = ./wallpaper.png;
}
