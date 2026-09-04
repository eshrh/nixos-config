{pkgs, ...}: {
  home.packages = [
    pkgs.alejandra
    pkgs.android-tools
    pkgs.bottom
    pkgs.cmake
    pkgs.jq
    pkgs.pandoc
    pkgs.ripgrep
    pkgs.eza
    pkgs.zoxide
    pkgs.zip
    pkgs.unzip
    pkgs.ispell
    pkgs.ncdu

    pkgs.fastfetch
    pkgs.ffmpeg
    pkgs.yt-dlp
    pkgs.imagemagick
  ];
  programs.qalculate = {
    enable = true;
    settings.Mode.calculate_as_you_type = 1;
  };
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  programs.git = {
    enable = true;
    settings = {
      user.name = "eshrh";
      user.email = "esrh@esrh.me";
      credential.helper = "store";
    };
  };
}
