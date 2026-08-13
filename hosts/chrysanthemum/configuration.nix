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

  # Keep the initrd LUKS prompt on the default US keymap. The shared
  # configuration sets console.keyMap to Dvorak, and NixOS also applies that
  # keymap before opening encrypted disks.
  console.keyMap = lib.mkForce "us";

  # The shared profile also uses Dvorak after X starts. Keep chrysanthemum on
  # US there as well, including the IBus XKB engine that Home Manager preloads.
  services.xserver.xkb = {
    layout = lib.mkForce "us";
    variant = lib.mkForce "";
  };

  home-manager.users.esrh.dconf.settings."org/freedesktop/ibus/general" = {
    preload-engines = lib.mkForce ["xkb:us::eng" "mozc-on" "rime"];
    engines-order = lib.mkForce ["xkb:us::eng" "mozc-on" "rime"];
  };

  system.stateVersion = "26.05"; # Did you read the comment?
}
