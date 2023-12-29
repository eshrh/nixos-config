{
  config,
  pkgs,
  ...
}: {
  programs.emacs.enable = true;
  home.sessionVariables = {
    EDITOR = "emacs";
  };
  xdg.configFile."emacs" = {
    source = ./conf;
    recursive = true;
  };
}
