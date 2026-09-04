{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    extraPackages = import ./packages.nix {inherit pkgs;};
  };

  home.sessionVariables.EDITOR = "emacs";

  xdg.configFile."emacs" = {
    source = ./conf;
    recursive = true;
  };
}
