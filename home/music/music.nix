{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./ncmpcpp.nix
  ];
  home.packages = [pkgs.mpc-cli];
  services.mpd = {
    enable = true;
    dataDir = "${config.xdg.configHome}/mpd";
    dbFile = "${config.xdg.configHome}/mpd/database";
    musicDirectory = "${config.home.homeDirectory}/mus";
    playlistDirectory = "${config.home.homeDirectory}/mus/playlists";
    network = {
      listenAddress = "127.0.0.1";
      port = 6600;
      startWhenNeeded = true;
    };
    extraConfig = ''
    audio_output {
        type      "httpd"
        name      "net_music"
        encoder   "vorbis"
        port      "8000"
        bitrate   "128"
        format    "44100:16:1"
        always_on "no"
        tags      "yes"
    }
    audio_output {
        type      "pulse"
        name      "pulse audio"
    }
    audio_output {
        type      "fifo"
        name      "my_fifo"
        path      "/tmp/mpd.fifo"
        format    "44100:16:2"
    }
    '';
  };
  programs.beets = {
    enable = true;
    mpdIntegration.port = 6600;
    settings = {
      plugins = "edit";
      directory = "${config.home.homeDirectory}/mus";
      library = "${config.xdg.configHome}/mpd/BEETSdb";
      import.move = "no";
      import.copy = "yes";
    };
  };
}
