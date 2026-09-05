# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "chrysanthemum"; # Define your hostname.

  console.keyMap = "us";

  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Keep the IBus engine consistent with the host XKB layout.
  home-manager.users.esrh.dconf.settings."org/freedesktop/ibus/general" = {
    preload-engines = lib.mkForce ["xkb:us::eng" "mozc-on" "rime"];
    engines-order = lib.mkForce ["xkb:us::eng" "mozc-on" "rime"];
  };

  services.picom = {
    enable = true;
    backend = "glx";
  };

  system.stateVersion = "26.05"; # Did you read the comment?
}
