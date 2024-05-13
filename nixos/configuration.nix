{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ./caps_to_esc.nix
    ./ibus.nix
    ./fonts/fonts.nix
    ./razer/razer.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices."luks-58e3c917-e5d1-4406-a6fa-acf832a930e0".device = "/dev/disk/by-uuid/58e3c917-e5d1-4406-a6fa-acf832a930e0";

  time.timeZone = "America/New_York";
  services.xserver.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.xkb = {
    layout = "us";
    variant = "dvorak";
  };
  console.keyMap = "dvorak";

  programs.fish.enable = true;
  users.users.esrh = {
    isNormalUser = true;
    description = "Eshan Ramesh";
    extraGroups = ["networkmanager" "wheel" "audio" "openrazer"];
    shell = pkgs.fish;
  };

  environment.systemPackages = with pkgs; [
    emacs
    git
  ];

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    audio.enable = true;
    pulse.enable = true;
  };

  nix.settings.experimental-features = ["nix-command" "flakes"];
  system.stateVersion = "23.11";
}
