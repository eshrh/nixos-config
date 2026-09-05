{...}: {
  systemd.tmpfiles.rules = [
    "d /disk/www 0755 root root -"
    "d /disk/www/decay.ng 0755 esrh nginx -"
  ];

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;

    virtualHosts."decay.ng" = {
      listen = [
        {
          addr = "127.0.0.1";
          port = 8080;
        }
      ];

      root = "/disk/www/decay.ng";

      locations."/".tryFiles = "$uri $uri/ =404";
    };
  };

  systemd.services.nginx = {
    requires = ["disk.mount"];
    after = ["disk.mount"];
  };
}
