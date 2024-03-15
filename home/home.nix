{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./emacs/emacs.nix
    ./xmonad/xmonad.nix
    ./xmobar/xmobar.nix
    ./fish/fish.nix
    ./shell_utils.nix
    ./gui_programs.nix
    ./dev.nix
    ./sioyek/sioyek.nix
    ./pulsemixer/pulsemixer.nix
    ./wallpaper/wallpaper.nix
    ./git.nix
    ./dunst/dunst.nix
    ./music/music.nix
  ];
  home.username = "esrh";
  home.homeDirectory = "/home/esrh";
  home.stateVersion = "23.05";

  programs.home-manager.enable = true;
}
