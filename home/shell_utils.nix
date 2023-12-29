{pkgs, ...}: {
  home.packages = [
    pkgs.ripgrep
    pkgs.ffmpeg
    pkgs.scrot
    pkgs.python3
    pkgs.pandoc
    pkgs.tree
    pkgs.xclip
    pkgs.zip
    pkgs.pywal
    pkgs.jq
    pkgs.neofetch
    pkgs.zoxide
    pkgs.cmake
    pkgs.mpv
    pkgs.killall
    pkgs.alejandra
    pkgs.waifu2x-converter-cpp
    pkgs.ncdu
    pkgs.pulseaudio  # for pactl
    pkgs.yt-dlp
  ];
}
