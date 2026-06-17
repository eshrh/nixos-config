{
  config,
  pkgs,
  ...
}: {
  imports = [./iosevka.nix];
  fonts = {
    packages = with pkgs; [
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-color-emoji
      # iosevka-meiseki # from ./iosevka.nix, custom iosevka build
      ioskeley-mono.normal
    ];
    fontconfig = {
      defaultFonts = {
        sansSerif = ["Noto Sans"];
        serif = ["Noto Serif"];
        monospace = ["Ioskeley Mono"];
        emoji = ["Noto Color Emoji"];
      };
    };
  };
}
