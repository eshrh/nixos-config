{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./hardware-configuration.nix];

  boot.initrd.luks.devices."luks-4e59cf21-e428-4754-8178-4630e943a4d0".device = "/dev/disk/by-uuid/4e59cf21-e428-4754-8178-4630e943a4d0";
  boot.kernelPackages = pkgs.linuxPackages_latest;

  services.fwupd.enable = true;
  systemd.timers.fwupd-refresh.enable = false;

  services.pipewire.wireplumber.extraConfig."51-freedsp-volume-limit" = {
    "monitor.alsa.rules" = [
      {
        matches = [
          {
            "device.name" = "~alsa_card.usb-MOONDROP_FreeDSP_Mini_.*";
          }
        ];
        actions.update-props = {
          # Avoid the FreeDSP's unreliable hardware volume control.
          "api.alsa.soft-mixer" = true;
        };
      }
      {
        matches = [
          {
            "node.name" = "~alsa_output.usb-MOONDROP_FreeDSP_Mini_.*";
          }
        ];
        actions.update-props = {
          # PulseAudio-style 50% volume: 0.5 ^ 3 = 0.125 (-18.1 dB).
          "channelmix.max-volume" = 0.125;
        };
      }
    ];
  };

  networking.hostName = "iris";

  system.stateVersion = "26.05";

  services.udev.extraRules = ''
    RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/intel_backlight/brightness"
    RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/intel_backlight/brightness"

    # moondrop freedsp mini
    KERNEL=="hidraw*", ATTRS{idVendor}=="35d8", ATTRS{idProduct}=="98d4", MODE="0666", TAG+="uaccess"
  '';

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };
}
