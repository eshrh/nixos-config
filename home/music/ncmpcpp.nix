{
  config,
  pkgs,
  ...
}: {
  programs.ncmpcpp = {
    enable = true;
    package = (pkgs.ncmpcpp.override { visualizerSupport = true; });
    bindings = [
      { key = "up"; command = "scroll_up"; }
      { key = "ctrl-p"; command = "scroll_up"; }
      { key = "t"; command = "scroll_up"; }
      { key = "down"; command = "scroll_down"; }
      { key = "ctrl-n"; command = "scroll_down"; }
      { key = "h"; command = "scroll_down"; }

      { key = "["; command = "scroll_up_album"; }
      { key = "]"; command = "scroll_down_album"; }
      { key = "{"; command = "scroll_up_artist"; }
      { key = "}"; command = "scroll_down_artist"; }

      { key = "space";
        command = ''require_screen "playlist" \n select item'';}

      { key = "n"; command = "next_column"; }
      { key = "n"; command = "slave_screen"; }
      { key = "d"; command = "previous_column"; }
      { key = "d"; command = "master_screen"; }

      { key = "8"; command = "show_visualizer"; }
      { key = "s"; command = "toggle_single"; }
      { key = "c"; command = "toggle_consume"; }
      { key = "-"; command = "find_item_forward"; }
      { key = "_"; command = "find_item_backward"; }
    ];
    settings = {
      ncmpcpp_directory = "${config.xdg.configHome}/ncmpcpp";
      mpd_host = "localhost";
      mpd_port = 6600;
      mpd_connection_timeout = 5;

      visualizer_data_source = "/tmp/mpd.fifo";
      visualizer_output_name = "my_fifo";
      visualizer_in_stereo = true;
      visualizer_type = "spectrum";
      visualizer_look = "●▮";
      visualizer_spectrum_dft_size = 2;
      visualizer_color = "blue, cyan, green, yellow, magenta, red";
      visualizer_spectrum_hz_min = 20;
      visualizer_spectrum_hz_max = 20000;
      visualizer_spectrum_smooth_look = false;

      playlist_shorten_total_times = true;
      playlist_display_mode = "columns";
      browser_display_mode = "columns";
      search_engine_display_mode = "columns";
      playlist_editor_display_mode = "columns";

      progressbar_look = "->";
      user_interface = "alternative";
      data_fetching_delay = false;
      media_library_primary_tag = "album_artist";
      media_library_albums_split_by_date = true;
      header_visibility = false;
      connected_message_on_startup = false;
      cyclic_scrolling = true;

      screen_switcher_mode = "playlist, media_library";
      startup_screen = "media_library";
      startup_slave_screen = "";
      startup_slave_screen_focus = false;

      locked_screen_width_part = 65;
      ask_before_clearing_playlists = false;
      display_volume_level = false;
      display_bitrate = false;
      ignore_diacritics = true;
      search_engine_default_search_mode = 2;
      use_console_editor = true;
    };
  };
}
