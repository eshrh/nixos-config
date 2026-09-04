{pkgs, ...}: {
  imports = [
    ./caps_to_esc.nix
    ./fonts/fonts.nix
    ./ibus.nix
    ./mullvad.nix
    ./nonfree.nix
    ./steam.nix
  ];

  services.xserver = {
    enable = true;
    windowManager.xmonad.enable = true;
    desktopManager.wallpaper.mode = "fill";
    xkb = {
      layout = "us";
      variant = "dvorak";
    };
  };

  users.users.esrh.extraGroups = ["audio" "video" "cdrom" "dialout"];

  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  services.hardware.bolt.enable = true;

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    audio.enable = true;
    pulse.enable = true;
  };

  hardware.keyboard.qmk.enable = true;
}
