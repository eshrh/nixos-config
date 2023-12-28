{
  config,
  pkgs,
  lib,
  ...
}: {
  environment.systemPackages = [
    pkgs.interception-tools
  ];
  services.interception-tools = let
    dfkConfig = pkgs.writeText "dual-function-keys.yaml" ''
      MAPPINGS:
        - KEY: KEY_CAPSLOCK
          TAP: KEY_ESC
          HOLD: KEY_LEFTCTRL
    '';
  in {
    enable = true;
    plugins = lib.mkForce [
      pkgs.interception-tools-plugins.dual-function-keys
    ];
    udevmonConfig = ''
      - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.dual-function-keys}/bin/dual-function-keys -c ${dfkConfig} | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
        DEVICE:
          LINK: /dev/input/by-path/platform-i8042-serio-0-event-kbd
          EVENTS:
            EV_KEY: [[KEY_CAPSLOCK, KEY_ESC, KEY_LEFTCTRL]]
    '';
  };
}
