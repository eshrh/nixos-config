{pkgs, ...}: {
  networking.hostName = "helianthus";
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  environment.systemPackages = [pkgs.mullvad-vpn pkgs.mullvad];
  services.mullvad-vpn.enable = true;
}
