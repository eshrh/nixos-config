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
      name = "Berkeley Mono";
      size = 12;
    };
    settings.confirm_os_window_close = 0;
    settings.auto_reload_config = -1;
    settings.bold_font = ''family="Berkeley Mono" style="Bold"'';
    settings.italic_font = ''family="Berkeley Mono" style="Oblique"'';
    settings.bold_italic_font = ''family="Berkeley Mono" style="Bold Oblique"'';
  };
  programs.firefox.enable = true;
}
