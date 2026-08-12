{pkgs, ...}: {
  home.packages = [
    (pkgs.python3.withPackages (python-pkgs: [
      python-pkgs.numpy
      python-pkgs.matplotlib
      python-pkgs.requests
      python-pkgs.ipython

    ]))
    pkgs.pyright
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

    pkgs.claude-code
    pkgs.codex
  ];
}
