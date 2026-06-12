{pkgs, ...}: {
  home.packages = [
    pkgs.anki
    pkgs.feh
    pkgs.gimp
    pkgs.libreoffice
    pkgs.qbittorrent
    pkgs.signal-desktop
    pkgs.thunderbird
    pkgs.obs-studio
    pkgs.foliate
    pkgs.xournalpp
  ];
  programs.alacritty = {
    enable = true;
    settings.font.size = 8;
  };
  programs.firefox.enable = true;
}
