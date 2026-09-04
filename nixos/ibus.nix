{
  config,
  pkgs,
  lib,
  ...
}: {
  i18n = {
    supportedLocales = lib.mkOptionDefault [
      "ja_JP.UTF-8/UTF-8"
      "ja_JP.EUC-JP/EUC-JP"
    ];
    inputMethod = {
      type = "ibus";
      enable = true;
      ibus.engines = with pkgs.ibus-engines; [mozc-ut rime];
    };
  };

  # ibus will not start automatically,
  # this option is necessary to run the autostart file that
  # creates the service for the ibus daemon.
  # https://wiki.archlinux.org/title/XDG_Autostart
  services.xserver.desktopManager.runXdgAutostartIfNone = true;
}
