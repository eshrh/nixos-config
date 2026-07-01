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
    pkgs.vscodium-fhs
    pkgs.xournalpp
    pkgs.yubioath-flutter
  ];
  programs.kitty = {
    enable = true;
    font = {
      name = "monospace";
      size = 12;
    };
    settings.confirm_os_window_close = 0;
    settings.auto_reload_config = -1;
  };
  programs.firefox.enable = true;
}
