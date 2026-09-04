{pkgs}: epkgs: let
  mkGitPackage = {
    pname,
    version,
    owner,
    repo,
    rev,
    hash,
    packageRequires ? [],
    files ? null,
  }:
    epkgs.melpaBuild (
      {
        inherit pname version packageRequires;
        src = pkgs.fetchFromGitHub {
          inherit owner repo rev hash;
        };
      }
      // pkgs.lib.optionalAttrs (files != null) {inherit files;}
    );

  nyaatouch = mkGitPackage {
    pname = "nyaatouch";
    version = "0.0.1-unstable-2026-03-18";
    owner = "eshrh";
    repo = "nyaatouch";
    rev = "574c8bf812224e0bd7a68f8ee0aa7c96d80e63fe";
    hash = "sha256-21A/ntv9668PDnVkJ1471cDhsj5bScgtxpAPhCrrmbs=";
    packageRequires = with epkgs; [
      avy
      meow
      meow-tree-sitter
      smartparens
      swiper
    ];
  };

  far = mkGitPackage {
    pname = "far";
    version = "0-unstable-2025-04-04";
    owner = "eshrh";
    repo = "far.el";
    rev = "4b60a95ad98d528736b6dfb20cefb7e59c29183a";
    hash = "sha256-KlCeN6UeTG6uCCpVTEp6eXr74/MWjHxX1Ogmx5cZx+Y=";
    packageRequires = with epkgs; [
      dash
      s
    ];
  };

  daml-ts-mode = mkGitPackage {
    pname = "daml-ts-mode";
    version = "0.0.1-unstable-2026-06-12";
    owner = "esrh-osec";
    repo = "daml-ts-mode.el";
    rev = "11af72c1f38be19da2a7cff1c185661a74ea09ad";
    hash = "sha256-WZhIqY4gkg90osNwEcSzWm5uFRq7VjvNPXfPbU2rm9k=";
    packageRequires = [epkgs.haskell-mode];
  };

  campus = mkGitPackage {
    pname = "campus";
    version = "0-unstable-2023-12-17";
    owner = "eshrh";
    repo = "campus-emacs";
    rev = "0a475cd7704001d8dc8280acb91a317db797933b";
    hash = "sha256-lVrNAfyUiotxpq/T52Dc0tVl7Sc6UKZLsE2ZOQlkfo8=";
    packageRequires = with epkgs; [
      dash
      s
    ];
  };

  matsurika-mode = mkGitPackage {
    pname = "matsurika-mode";
    version = "0-unstable-2022-10-06";
    owner = "eshrh";
    repo = "matsurika-mode";
    rev = "db3d313d9b76af3fdff46d5510ba928c19fe1ca4";
    hash = "sha256-7Q11/DGfG9vn2qoB/n60t5CRkkgX/Me66ghYkhQnB4g=";
    packageRequires = [epkgs.clojure-mode];
    files = ''(:defaults "docs.txt")'';
  };

  damlGrammar = pkgs.tree-sitter.buildGrammar {
    language = "daml";
    version = "0.23.1";
    src = pkgs.fetchFromGitHub {
      owner = "Artifex1";
      repo = "tree-sitter-daml";
      rev = "89955ac59bc9b78598339dc56cb1729db90124aa";
      hash = "sha256-6fKVQR7KwvtWy96Oz0vObDS2DC4u9edICvoSv14xsHc=";
    };
  };

  treeSitterGrammars = epkgs.treesit-grammars.with-grammars (grammars:
    (with grammars; [
      tree-sitter-bash
      tree-sitter-c
      tree-sitter-cpp
      tree-sitter-haskell
      tree-sitter-html
      tree-sitter-java
      tree-sitter-javascript
      tree-sitter-julia
      tree-sitter-python
      tree-sitter-rust
      tree-sitter-tsx
      tree-sitter-typescript
    ])
    ++ [damlGrammar]);
in
  (with epkgs; [
    ace-window
    aggressive-indent
    agda2-mode
    auctex
    cider
    clojure-mode
    corfu
    dashboard
    deadgrep
    elisp-format
    envrc
    fish-mode
    flycheck
    gruvbox-theme
    haskell-mode
    helpful
    highlight-defined
    highlight-numbers
    highlight-quoted
    hl-todo
    ivy-bibtex
    ligature
    magit
    marginalia
    markdown-mode
    nix-mode
    orderless
    org-fragtog
    org-ref
    outline-magic
    projectile
    racket-mode
    rainbow-delimiters
    rainbow-mode
    rust-mode
    scribble-mode
    slime
    slime-company
    smartparens
    telephone-line
    typescript-mode
    undo-tree
    vertico
    vertico-posframe
    vterm
    vterm-toggle
    yasnippet
  ])
  ++ [
    campus
    daml-ts-mode
    far
    matsurika-mode
    nyaatouch
    treeSitterGrammars
  ]
