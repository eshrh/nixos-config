{
  config,
  pkgs,
  ...
}: {
  imports = [./iosevka.nix];
  fonts = {
    packages = with pkgs; [
      noto-fonts
      noto-fonts-emoji
      ipafont
      iosevka-meiseki # from ./iosevka.nix, custom iosevka build
    ];
    fontconfig = {
      defaultFonts = {
        sansSerif = ["Noto Sans" "IPAGothic"];
        serif = ["Noto Serif" "IPAMincho"];
        monospace = ["Iosevka Meiseki" "IPAGothic"];
        emoji = ["Noto Color Emoji"];
      };
    };
  };
}
