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
}
