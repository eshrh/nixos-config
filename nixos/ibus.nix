{
  config,
  pkgs,
  lib,
  ...
}: {
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT = "en_US.UTF-8";
      LC_MONETARY = "en_US.UTF-8";
      LC_NAME = "en_US.UTF-8";
      LC_NUMERIC = "en_US.UTF-8";
      LC_PAPER = "en_US.UTF-8";
      LC_TELEPHONE = "en_US.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };
    supportedLocales = lib.mkOptionDefault [
      "ja_JP.UTF-8/UTF-8"
      "ja_JP.EUC-JP/EUC-JP"

    ];
    inputMethod = {
      type = "ibus";
      enable = true;
      ibus.engines = with pkgs.ibus-engines; [mozc-ut];
    };
  };

  # ibus will not start automatically,
  # this option is necessary to run the autostart file that
  # creates the service for the ibus daemon.
  # https://wiki.archlinux.org/title/XDG_Autostart
  services.xserver.desktopManager.runXdgAutostartIfNone = true;
}
