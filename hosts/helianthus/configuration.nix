{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [ ./hardware-configuration.nix ];

  boot.initrd.luks.devices."luks-58e3c917-e5d1-4406-a6fa-acf832a930e0".device = "/dev/disk/by-uuid/58e3c917-e5d1-4406-a6fa-acf832a930e0";
  boot.kernelParams = ["amdgpu.backlight=0" "acpi_backlight=video"];

  networking.hostName = "helianthus";
  system.stateVersion = "23.11";

  services.udev.extraRules = ''
    RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/acpi_video0/brightness"
    RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/acpi_video0/brightness"
  '';
}
