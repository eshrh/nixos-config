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
    ]))
  ];
}
