{
  config,
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.sioyek
  ];
  xdg.configFile."sioyek" = {
    source = ./conf;
    recursive = true;
  };
  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    "application/pdf" = [ "sioyek.desktop" ];
  };
}
