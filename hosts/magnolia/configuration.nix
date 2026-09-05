{pkgs, ...}: {
  imports = [./hardware-configuration.nix];

  networking.hostName = "magnolia";

  fileSystems."/disk" = {
    device = "/dev/disk/by-label/ssd";
    fsType = "ext4";
    options = ["noatime"];
  };

  services.fstrim.enable = true;

  networking.networkmanager.dispatcherScripts = [
    {
      source = pkgs.writeShellScript "magnolia-wifi-fallback" ''
        if [ "$1" != "eno2" ]; then
          exit 0
        fi

        nmcli=${pkgs.networkmanager}/bin/nmcli
        wired_state="$($nmcli --get-values GENERAL.STATE device show eno2 2>/dev/null || true)"
        wired_gateway="$($nmcli --get-values IP4.GATEWAY device show eno2 2>/dev/null || true)"

        case "$wired_state" in
          100*)
            if [ -n "$wired_gateway" ]; then
              $nmcli --wait 0 device down wlo1 || true
              exit 0
            fi
            ;;
        esac

        $nmcli --wait 0 device up wlo1 || true
      '';
    }
  ];

  console.keyMap = "us";
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  system.stateVersion = "26.05";
}
