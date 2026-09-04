{pkgs, ...}: {
  home.packages = [
    pkgs.pulseaudio # for pactl
    pkgs.pulsemixer
    pkgs.libnotify
    pkgs.pywal
    pkgs.mpv
    pkgs.flameshot
    pkgs.xclip
  ];

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
     mode2     = KEY_F2, r
     mode3     = KEY_F3, l
  '';
}
