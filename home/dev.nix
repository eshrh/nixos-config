{pkgs, ...}: {
  home.packages = [
    (pkgs.python3.withPackages (python-pkgs: [
      python-pkgs.pandas
      python-pkgs.numpy
      python-pkgs.matplotlib
      python-pkgs.requests
      python-pkgs.ipython
      python-pkgs.pyqt6

      python-pkgs.python-lsp-server
      python-pkgs.pylsp-mypy
      python-pkgs.pylsp-rope
    ]))
    pkgs.gnumake
    pkgs.gcc
    pkgs.libtool

    pkgs.ghc
    pkgs.stack
    pkgs.haskellPackages.haskell-language-server

    pkgs.racket

    pkgs.cargo
    pkgs.rustc
    pkgs.rust-analyzer
  ];
}
