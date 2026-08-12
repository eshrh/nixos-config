{
  config,
  pkgs,
  ...
}: let
  emacs = config.programs.emacs.finalPackage;
  # Tangle init.org -> init.el at build time using org-babel-tangle.
  tangledInit = pkgs.runCommand "init.el" {} ''
    cp ${./conf/init.org} init.org
    ${emacs}/bin/emacs --batch \
      --eval "(require 'org)" \
      --eval '(org-babel-tangle-file "init.org")'
    cp init.el $out
  '';
in {
  programs.emacs = {
    enable = true;

    # in general, I use straight for package management.
    # however, agda-mode comes from the agda source code itself.
    # so, it's quite annoying to install via straight.
    extraPackages = epkgs: [
      epkgs.agda2-mode
    ];
  };

  home.sessionVariables = {
    EDITOR = "emacs";
  };
  xdg.configFile."emacs" = {
    source = ./conf;
    recursive = true;
  };
  xdg.configFile."emacs/init.el".source = tangledInit;
}
