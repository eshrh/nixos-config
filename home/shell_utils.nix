{pkgs, ...}: {
  home.packages = [
    pkgs.alejandra
    pkgs.cmake
    pkgs.jq
    pkgs.pandoc
    pkgs.ripgrep
    pkgs.killall
    pkgs.zoxide
    pkgs.zip
    pkgs.unzip
    pkgs.ispell
    pkgs.ncdu
    pkgs.pulseaudio  # for pactl
    pkgs.pulsemixer
    pkgs.cloudflared

    pkgs.neofetch
    pkgs.pywal
    pkgs.ffmpeg
    pkgs.mpv
    pkgs.flameshot
    pkgs.xclip
    pkgs.yt-dlp
    pkgs.imagemagick

    pkgs.texliveFull
    pkgs.libqalculate
  ];
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  xdg.configFile."pulsemixer.cfg".text = ''
    [general]
     step = 1
     step-big = 10
    [keys]
     up        = k, KEY_UP, KEY_PPAGE, t
     down      = j, KEY_DOWN, KEY_NPAGE, h
     left      = h, KEY_LEFT, d
     right     = l, KEY_RIGHT, n
     mode1     = KEY_F1, c
     mode2     = KEY_F2, l
     mode3     = KEY_F3, r
  '';
  programs.git = {
    enable = true;
    userName  = "eshrh";
    userEmail = "esrh@esrh.me";
  };
}
