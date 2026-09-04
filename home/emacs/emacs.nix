{
  lib,
  pkgs,
  ...
}: {
  programs.emacs = {
    enable = true;
    package = lib.mkDefault pkgs.emacs-nox;
    extraPackages = import ./packages.nix {inherit pkgs;};
  };

  home.sessionVariables.EDITOR = "emacs";

  xdg.configFile."emacs" = {
    source = ./conf;
    recursive = true;
  };
}
