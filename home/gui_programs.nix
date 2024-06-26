{pkgs, ...}:
 {
  home.packages = [
    pkgs.android-tools
    pkgs.anki
    pkgs.feh
    pkgs.gimp
    pkgs.libreoffice
    pkgs.qbittorrent
    pkgs.signal-desktop
    pkgs.thunderbird
    pkgs.zotero
    pkgs.obs-studio
    pkgs.gnuradio
    pkgs.uhd
    pkgs.foliate
    pkgs.osu-lazer-bin
    pkgs.zoom-us
    pkgs.teams-for-linux
  ];
  programs = {
    alacritty.enable = true;
    firefox.enable = true;
  };
}
