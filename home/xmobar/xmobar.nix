{
  config,
  pkgs,
  ...
}: {
  home.packages = [
    (let
      ghc' = pkgs.haskellPackages.ghcWithPackages (self: [
        self.xmobar
        self.xmonad
        self.xmonad-contrib
      ]);
    in
      (pkgs.runCommandLocal "xmonad-compile" {
        nativeBuildInputs = [ghc'];
      }) ''
        # FIXME: proper paths
        cp ${./conf/xmobar.hs} xmobar.hs
        cp ${./conf/WeatherStem.hs} WeatherStem.hs

        ghc --make -threaded xmobar.hs

        install -Dm755 xmobar -T $out/bin/xmobar
      '')
  ];
}
