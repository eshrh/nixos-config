final: prev: let
  self = prev.ibus-engines;
  mozcdic = final.mozcdic-ut.override {
    alt-cannadic = true;
    edict = true;
    skk-jisyo = true;
    sudachidict = true;
  };
in {
  ibus-engines =
    self
    // {
      mozc-ut = self.mozc.overrideAttrs (previousAttrs: {
        preBuild =
          previousAttrs.preBuild
          or ""
          + ''
            # Add the UT dictionary
            cat ${mozcdic}/mozcdic-ut.txt >> data/dictionary_oss/dictionary00.txt
          '';
      });
    };
}
