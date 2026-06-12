{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [ ./hardware-configuration.nix ];

  boot.initrd.luks.devices."luks-4e59cf21-e428-4754-8178-4630e943a4d0".device = "/dev/disk/by-uuid/4e59cf21-e428-4754-8178-4630e943a4d0";
  boot.kernelPackages = pkgs.linuxPackages_latest;

  services.fwupd.enable = true;
  systemd.timers.fwupd-refresh.enable = false;

  networking.hostName = "iris";

  system.stateVersion = "26.05";

  services.udev.extraRules = ''
    RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/intel_backlight/brightness"
    RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/intel_backlight/brightness"
  '';
}
