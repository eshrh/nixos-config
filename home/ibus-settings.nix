{...}: {
  # for ibus... see /nixos/ibus.nix
  dconf.settings = {
    "org/freedesktop/ibus/general" = {
      preload-engines = ["xkb:us:dvorak:eng" "mozc-on" "rime"];
      engines-order = ["xkb:us:dvorak:eng" "mozc-on" "rime"];
    };
  };

  # mozc engine config. The default ships with its own candidate-window
  # renderer (mozc_renderer) enabled, which fails to start under bare xmonad
  # (none+xmonad), so candidates never display. Disable it to fall back to
  # IBus' built-in candidate window.
  xdg.configFile."mozc/ibus_config.textproto".text = ''
    engines {
      name : "mozc-jp"
      longname : "Mozc"
      layout : "default"
      rank : 80
      symbol : "あ"
    }
    engines {
      name : "mozc-on"
      longname : "Mozc:あ"
      layout : "default"
      rank : 99
      symbol : "あ"
      composition_mode : HIRAGANA
    }
    engines {
      name : "mozc-off"
      longname : "Mozc:A_"
      layout : "default"
      rank : 99
      symbol : "A"
      composition_mode : DIRECT
    }
    active_on_launch: False
    mozc_renderer {
      enabled : False
    }
  '';
}
