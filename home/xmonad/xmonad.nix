{
  config,
  pkgs,
  ...
}: {
  xsession = {
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./conf/xmonad.hs;
      libFiles = {"Brightness.hs" = ./conf/Brightness.hs;};
      extraPackages = haskellPackages: [
        haskellPackages.split
        haskellPackages.data-default
        haskellPackages.extra
      ];
    };
  };
}
