{pkgs, ...}: {
  home.packages = [
    pkgs.alejandra
    pkgs.cmake
    pkgs.ffmpeg
    pkgs.jq
    pkgs.killall
    pkgs.mpv
    pkgs.ncdu
    pkgs.neofetch
    pkgs.pandoc
    pkgs.pulseaudio  # for pactl
    pkgs.python3
    pkgs.pywal
    pkgs.ripgrep
    pkgs.scrot
    pkgs.tree
    pkgs.waifu2x-converter-cpp
    pkgs.xclip
    pkgs.yt-dlp
    pkgs.zip
    pkgs.zoxide
  ];
}
