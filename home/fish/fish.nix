{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.fish.enable = true;
  home.packages = [pkgs.fishPlugins.z];
  xdg.configFile."fish/config.fish".source = lib.mkForce ./conf/config.fish;
}
