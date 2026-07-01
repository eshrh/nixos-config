{ pkgs }:
pkgs.stdenv.mkDerivation {
  pname = "berkeley-mono";
  version = "2.004";

  src = ./tx02.tar.gz;

  unpackPhase = ''
    runHook preUnpack
    ${pkgs.tar}/bin/tar -xzvf $src
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall
    install -Dm644 TX-02/*.otf -t $out/share/fonts/opentype
    runHook postInstall
  '';
}
