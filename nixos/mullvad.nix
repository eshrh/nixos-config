{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = [pkgs.mullvad-vpn pkgs.mullvad];
  services.mullvad-vpn.enable = true;

  systemd.services.mullvad-allow-lan = {
    description = "Allow LAN traffic through Mullvad";
    wantedBy = ["multi-user.target"];
    requires = ["mullvad-daemon.service"];
    after = ["mullvad-daemon.service"];
    partOf = ["mullvad-daemon.service"];
    startLimitBurst = 30;
    startLimitIntervalSec = 60;

    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${lib.getExe config.services.mullvad-vpn.package} lan set allow";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = 1;
    };
  };
}
