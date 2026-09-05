{pkgs, ...}: {
  systemd.tmpfiles.rules = [
    "d /var/lib/cloudflared 0700 root root -"
  ];

  systemd.services.cloudflared = {
    description = "Cloudflare Tunnel";
    wantedBy = ["multi-user.target"];
    wants = ["network-online.target"];
    after = ["network-online.target"];

    serviceConfig = {
      DynamicUser = true;
      LoadCredential = "tunnel-token:/var/lib/cloudflared/tunnel-token";
      ExecStart = "${pkgs.cloudflared}/bin/cloudflared tunnel --no-autoupdate run --token-file %d/tunnel-token";
      Restart = "always";
      RestartSec = 5;

      CapabilityBoundingSet = "";
      LockPersonality = true;
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectHome = true;
      ProtectSystem = "strict";
    };
  };
}
