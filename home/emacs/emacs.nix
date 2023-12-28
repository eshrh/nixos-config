{
  config,
  pkgs,
  ...
}: {
  programs.emacs.enable = true;
  home.sessionVariables = {
    EDITOR = "emacs";
  };
  home.file."${config.xdg.configHome}/emacs" = {
    source = ./conf;
    recursive = true;
  };
}
