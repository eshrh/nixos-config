{
  config,
  pkgs,
  ...
}: {
  services.dunst = {
    enable = true;
    configFile = ./conf/dunstrc;
  };
}
