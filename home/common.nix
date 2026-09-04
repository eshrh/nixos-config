{...}: {
  imports = [
    ./dev.nix
    ./emacs/emacs.nix
    ./fish/fish.nix
    ./shell_utils.nix
  ];

  home.username = "esrh";
  home.homeDirectory = "/home/esrh";
  home.stateVersion = "26.05";
  programs.home-manager.enable = true;
}
