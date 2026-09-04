{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./common.nix
    ./desktop_utils.nix
    ./xmonad/xmonad.nix
    ./xmobar/xmobar.nix
    ./nsxiv/nsxiv.nix
    ./gui_programs.nix
    ./sioyek/sioyek.nix
    ./wallpaper/wallpaper.nix
    ./dunst/dunst.nix
    ./music/music.nix
    ./ibus-settings.nix
    ./nonfree/claude.nix
    ./texlive.nix
  ];

  programs.emacs.package = pkgs.emacs;

  xdg.userDirs = {
    enable = true;
    createDirectories = false;
    setSessionVariables = false;
    documents = "${config.home.homeDirectory}/docs";
    download = "${config.home.homeDirectory}/down";
    music = "${config.home.homeDirectory}/muse";
    pictures = "${config.home.homeDirectory}/pics";
    videos = "${config.home.homeDirectory}/vids";
    desktop = "/tmp/.desktop";
    templates = "/tmp/.templates";
    publicShare = "/tmp/.publicshare";
  };
}
