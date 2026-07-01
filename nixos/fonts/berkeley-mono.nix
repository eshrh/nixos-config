{ pkgs }:
pkgs.stdenv.mkDerivation {
  pname = "berkeley-mono-typeface";
  version = "2.004";

  src = ./tx02.tar.gz;

  unpackPhase = ''
    runHook preUnpack
    ${pkgs.gnutar}/bin/tar -xzvf $src
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall
    install -Dm644 TX-02/*.ttf -t $out/share/fonts/truetype
    runHook postInstall
  '';
}
