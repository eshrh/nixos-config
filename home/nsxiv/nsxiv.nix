{pkgs, ...}: {
  home.packages = [
    pkgs.nsxiv
  ];
  xdg.configFile."nsxiv" = {
    source = ./conf;
    recursive = true;
  };
}
