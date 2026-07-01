{
  config,
  pkgs,
  ...
}:
let
  berkeley-mono = pkgs.callPackage ./berkeley-mono.nix {};
in
{
  imports = [./iosevka.nix];
  fonts = {
    packages = with pkgs; [
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-color-emoji
      inter
      berkeley-mono
      # iosevka-meiseki # from ./iosevka.nix, custom iosevka build
    ];
    fontconfig = {
      defaultFonts = {
        sansSerif = ["Inter Variable"];
        serif = ["Noto Serif"];
        monospace = ["Berkeley Mono" "Noto Sans Mono"];
        emoji = ["Noto Color Emoji"];
      };
    };
  };
}
