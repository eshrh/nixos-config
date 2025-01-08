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
    pkgs.cloudflared

    pkgs.neofetch
    pkgs.pywal
    pkgs.ffmpeg
    pkgs.mpv
    pkgs.scrot
    pkgs.waifu2x-converter-cpp
    pkgs.xclip
    pkgs.yt-dlp
    pkgs.imagemagick

    pkgs.texliveFull
    pkgs.libqalculate
    pkgs.octave
    pkgs.sage
    pkgs.sagetex
  ];
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
