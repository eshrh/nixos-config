{
  config,
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.sioyek
  ];
  home.file."${config.xdg.configHome}/sioyek" = {
    source = ./conf;
    recursive = true;
  };
}
