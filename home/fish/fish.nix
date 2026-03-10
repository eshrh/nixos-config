{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.fish.enable = true;
  home.packages = [
    pkgs.fishPlugins.z
    pkgs.fishPlugins.gruvbox
  ];
  xdg.configFile."fish/config.fish".source = lib.mkForce ./conf/config.fish;
}
