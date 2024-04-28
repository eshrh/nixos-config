{pkgs, ...}: {
  home.packages = [
    (pkgs.python3.withPackages (python-pkgs: [
      python-pkgs.pandas
      python-pkgs.numpy
      python-pkgs.scipy
      python-pkgs.scikit-learn
      python-pkgs.matplotlib
      python-pkgs.seaborn
      python-pkgs.requests
      python-pkgs.ipython
      python-pkgs.pyarrow
      python-pkgs.pyqt6
      python-pkgs.pyzmq

      python-pkgs.python-lsp-server
      python-pkgs.pylsp-mypy
      python-pkgs.pylsp-rope

      python-pkgs.tensorflow-bin
      python-pkgs.torch
      python-pkgs.torchvision
      python-pkgs.jax
      python-pkgs.jaxlib

    ]))
    pkgs.gnumake
    pkgs.gcc

    pkgs.ghc
    pkgs.racket
  ];
}
