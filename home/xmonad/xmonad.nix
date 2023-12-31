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
      extraPackages = haskellPackages: [
        haskellPackages.split
        haskellPackages.data-default
        haskellPackages.extra
      ];
    };
  };
}
