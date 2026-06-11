{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [ ./hardware-configuration.nix ];

  boot.initrd.luks.devices."luks-4e59cf21-e428-4754-8178-4630e943a4d0".device = "/dev/disk/by-uuid/4e59cf21-e428-4754-8178-4630e943a4d0";
  networking.hostName = "iris";
}
