{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  fetchurl,
  ibus-engines,
  mozc ? ibus-engines.mozc,
  glibcLocales,
  ruby,
  alt-cannadic ? false,
  edict ? false,
  jawiki ? true,
  neologd ? true,
  personal-names ? true,
  place-names ? true,
  skk-jisyo ? false,
  sudachidict ? false,
}:
# see https://github.com/utuhiro78/merge-ut-dictionaries/blob/main/src/make.sh
# and https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=mozc-ut
stdenvNoCC.mkDerivation rec {
  pname = "mozcdic-ut";
  version = "20231220";

  srcs =
    [
      mozc.src
      (fetchFromGitHub rec {
        name = repo;
        owner = "utuhiro78";
        repo = "merge-ut-dictionaries";
        rev = "254e25281b363d28d992caa9985ef946b31146f1";
        hash = "sha256-21NqcOKpH6N2RhHzOCjCesWNwLWA6BGOExPo7N82V+4=";
      })
      (fetchurl {
        url = "https://dumps.wikimedia.org/jawiki/${version}/jawiki-${version}-all-titles-in-ns0.gz";
        hash = "sha256-EzbHb3piQjKy4goQtCweKRLhCPT3O35MpLJXt0PEKEM=";
      })
    ]
    ++ lib.optional alt-cannadic (fetchFromGitHub rec {
      name = repo;
      owner = "utuhiro78";
      repo = "mozcdic-ut-alt-cannadic";
      rev = "4e548e6356b874c76e8db438bf4d8a0b452f2435";
      hash = "sha256-4gzqVoCIhC0k3mh0qbEr8yYttz9YR0fItkFNlu7cYOY=";
    })
    ++ lib.optional edict (fetchFromGitHub rec {
      name = repo;
      owner = "utuhiro78";
      repo = "mozcdic-ut-edict2";
      rev = "ac4d4af457d891e865592258c4018f77b50c4fe6";
      hash = "sha256-JH7JEp5boblSUfQBT2KVBLtkQDfst912UzCelLrDIjE=";
    })
    ++ lib.optional jawiki (fetchFromGitHub rec {
      name = repo;
      owner = "utuhiro78";
      repo = "mozcdic-ut-jawiki";
      rev = "fe41386ae52c673b96827ab5e55e0aa18b66cb57";
      hash = "sha256-v9QNZc+coCcLg8TsFW+r508/XbKZH/O+ZyxcP/LEm+4=";
    })
    ++ lib.optional neologd (fetchFromGitHub rec {
      name = repo;
      owner = "utuhiro78";
      repo = "mozcdic-ut-neologd";
      rev = "be2a153f413dee01b8fa0cfdced845dcd1509b1d";
      hash = "sha256-6JBXrQKMxfk8VZZL1JMIziW+R0V97oZKsvnpiqu9xAs=";
    })
    ++ lib.optional personal-names (fetchFromGitHub rec {
      name = repo;
      owner = "utuhiro78";
      repo = "mozcdic-ut-personal-names";
      rev = "e939ab92959eec279ab98c53b6bd1273ed7e5acc";
      hash = "sha256-UzrLBx/4Dsi0ZZO5yBe3a32Ifsb4CuJGFSpPvNfChmU=";
    })
    ++ lib.optional place-names (fetchFromGitHub rec {
      name = repo;
      owner = "utuhiro78";
      repo = "mozcdic-ut-place-names";
      rev = "d57f84c88455cd81f1439aed0f3ec43cea4d7be6";
      hash = "sha256-iFznaeBz560Zk6aqQ7x6kesX0aIbK4W6WqXzC5iHnpU=";
    })
    ++ lib.optional skk-jisyo (fetchFromGitHub rec {
      name = repo;
      owner = "utuhiro78";
      repo = "mozcdic-ut-skk-jisyo";
      rev = "ee94f6546ce52edfeec0fd203030f52d4d99656f";
      hash = "sha256-RXxO878ZBkxenrdo7cFom5NjM0m7CdYQk0dFu/HPp/Y=";
    })
    ++ lib.optional sudachidict (fetchFromGitHub rec {
      name = repo;
      owner = "utuhiro78";
      repo = "mozcdic-ut-sudachidict";
      rev = "b35267f2cdb3d07876fb0581df4c7cf75af7ddd4";
      hash = "sha256-IShm2+WNtQUM4QN7DAaMl7IFKB5wOi5lR5puADcq/mo=";
    });

  sourceRoot = ".";

  # see pkgs/tools/archivers/unrar/setup-hook.sh
  # and https://www.reddit.com/r/NixOS/comments/kqe57g/
  preUnpack = ''
    unpackCmdHooks+=(_tryGz)
    _tryGz() {
      if [[ "$curSrc" =~ \.tar.gz$ ]]; then return 1; fi
      if ! [[ "$curSrc" =~ \.gz$ ]]; then return 1; fi
      cp "$curSrc" "$(stripHash "$curSrc")"
    }
  '';

  postUnpack = ''
    root="merge-ut-dictionaries/src"
    mv "${mozc.src.name}" "$root/mozc"
    mv jawiki-*-all-titles-in-ns0.gz "$root/jawiki-latest-all-titles-in-ns0.gz"
    mv mozcdic-ut-* "$root"
    cd "$root"
  '';

  patches = [
    ./local-jawiki.patch
    ./local-mozc.patch
    ./local-mozcdic-ut.patch
  ];

  configurePhase = ''
    runHook preConfigure

    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    export LANG=ja_JP.UTF-8

    runHook postConfigure
  '';

  strictDeps = true;
  nativeBuildInputs = [ruby];

  buildPhase = ''
    runHook preBuild

    bash make.sh

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    install -Dm644 mozcdic-ut.txt -t $out

    runHook postInstall
  '';

  meta = with lib; {
    description = "Multiple Mozc UT dictionaries merged into one dictionary";
    homepage = "http://linuxplayers.g1.xrea.com/mozc-ut.html";
    license = licenses.asl20;
    platforms = platforms.linux;
    maintainers = with maintainers; [stephen-huan];
  };
}
