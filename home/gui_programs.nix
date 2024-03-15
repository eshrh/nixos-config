{pkgs, ...}: {
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
    pkgs.gnuradio
  ];
  programs = {
    alacritty.enable = true;
    firefox.enable = true;
  };
}
