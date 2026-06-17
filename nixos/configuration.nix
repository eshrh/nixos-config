{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./networking.nix
    ./caps_to_esc.nix
    ./ibus.nix
    ./fonts/fonts.nix
    ./razer/razer.nix
    ./nonfree.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "Asia/Tokyo";
  services.xserver.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.desktopManager.wallpaper.mode = "fill";
  services.xserver.xkb = {
    layout = "us";
    variant = "dvorak";
  };
  console.keyMap = "dvorak";

  users.users.esrh = {
    isNormalUser = true;
    description = "Eshan Ramesh";
    extraGroups = [
      "networkmanager"
      "wheel"
      "audio"
      "video"
      "openrazer"
      "cdrom"
      "dialout"
    ];
    shell = pkgs.fish;
  };

  environment.systemPackages = with pkgs; [
    emacs
    git
    wine
    wine64
    wineWow64Packages.stable
    winetricks
    winePackages.stableFull
  ];

  programs = {
    fish.enable = true;
    steam.enable = true;
    gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };
  };

  services.openssh.enable = true;

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    audio.enable = true;
    pulse.enable = true;
  };

  # virtualisation.docker.enable = true;

  nix.settings.experimental-features = ["nix-command" "flakes"];
}
