{pkgs, ...}: let
  thunderbird-version = "154.0b4";

  thunderbird-154-unwrapped = pkgs.thunderbird-latest-bin-unwrapped.override {
    generated = {
      version = thunderbird-version;
      sources = [
        {
          url = "https://archive.mozilla.org/pub/thunderbird/releases/${thunderbird-version}/linux-x86_64/en-US/thunderbird-${thunderbird-version}.tar.xz";
          locale = "en-US";
          arch = "linux-x86_64";
          sha256 = "b25425577d524795466e7c1fc127f8a2d7591083628daa7eba0ced63a5a9cbae";
        }
      ];
    };
  };

  thunderbird-154 = pkgs.wrapThunderbird thunderbird-154-unwrapped {
    pname = "thunderbird-154";
    libName = "thunderbird-bin-${thunderbird-version}";
  };
in {
  home.packages = [
    pkgs.anki
    pkgs.feh
    pkgs.gimp
    pkgs.libreoffice
    pkgs.qbittorrent
    pkgs.signal-desktop
    thunderbird-154
    pkgs.obs-studio
    pkgs.foliate
    pkgs.vscodium-fhs
    pkgs.xournalpp
    pkgs.yubioath-flutter
    pkgs.solaar
    pkgs.wineWow64Packages.stableFull
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
