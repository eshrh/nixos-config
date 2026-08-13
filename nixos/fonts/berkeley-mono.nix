{ pkgs }:
pkgs.stdenv.mkDerivation {
  pname = "berkeley-mono-typeface";
  version = "2.004";

  src = pkgs.requireFile {
    name = "tx02.tar.gz";
    hash = "sha256-7tuSs/rmUjCeetWMvVBnVMgpjJ/mQYsdvzUtSPB2S6k=";
    message = "nix-store --add-fixed sha256 /path/to/tx02.tar.gz";
  };

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
