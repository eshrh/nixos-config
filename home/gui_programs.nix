{pkgs, ...}: {
  home.packages = [
    pkgs.feh
    pkgs.anki
    pkgs.android-tools
    pkgs.signal-desktop
    pkgs.libreoffice
    pkgs.qbittorrent
    pkgs.zotero
    pkgs.gimp
  ];
  programs.alacritty.enable = true;
  programs.firefox.enable = true;
}
