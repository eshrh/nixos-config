{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.fish.enable = true;
  home.packages = [pkgs.fishPlugins.z];
  home.file."${config.xdg.configHome}/fish/config.fish".source = lib.mkForce ./conf/config.fish;
}
