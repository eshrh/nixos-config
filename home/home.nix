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
    ./nsxiv/nsxiv.nix
    ./gui_programs.nix
    ./dev.nix
    ./sioyek/sioyek.nix
    ./wallpaper/wallpaper.nix
    ./dunst/dunst.nix
    ./music/music.nix
  ];
  home.username = "esrh";
  home.homeDirectory = "/home/esrh";
  home.stateVersion = "26.05";
  programs.home-manager.enable = true;

  # for ibus... see /nixos/ibus.nix
  dconf.settings = {
    "org/freedesktop/ibus/general" = {
      preload-engines = ["xkb:us:dvorak:eng" "mozc-jp" "rime"];
      engines-order = ["xkb:us:dvorak:eng" "mozc-jp" "rime"];
    };
  };

  xdg.userDirs = {
    enable = true;
    createDirectories = false;
    setSessionVariables = false;
    documents = "${config.home.homeDirectory}/docs";
    download = "${config.home.homeDirectory}/down";
    music = "${config.home.homeDirectory}/mus";
    pictures = "${config.home.homeDirectory}/pics";
    videos = "${config.home.homeDirectory}/vids";
    desktop = "/tmp/.desktop";
    templates = "/tmp/.templates";
    publicShare = "/tmp/.publicshare";
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
