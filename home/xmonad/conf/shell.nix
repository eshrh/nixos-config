{pkgs ? import <nixpkgs> {}}: let
  inherit (pkgs) haskellPackages;

  xmonadGhc = haskellPackages.ghcWithPackages (hp:
    with hp; [
      xmonad
      xmonad-contrib
      xmonad-extras

      split
      data-default
      extra
    ]);
in
  pkgs.mkShell {
    packages = [
      xmonadGhc
      haskellPackages.xmonad
      haskellPackages.cabal-install
      haskellPackages.ghcid
      haskellPackages.hlint
    ];

    shellHook = ''
      echo "xmonad Haskell shell"
      echo "  ghci xmonad.hs"
      echo "  ghcid --command 'ghci xmonad.hs'"
      echo "  xmonad --recompile"
    '';
  }
