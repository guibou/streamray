{ pkgs ? import ./nixpkgs.nix { } }: rec {
  inherit pkgs;

  # Explicit list of used files. Else there is always too much and
  # cache is invalidated.
  sources = pkgs.lib.sourceByRegex ./. [
    ".*\\.cabal$"
    ".*\\.hs$"
    ".*\\.md$"
    "src"
    "src/Streamray"
    "app"
    "tests"
    "LICENSE"
  ];

  shell = (app.envFunc { withHoogle = false; }).overrideAttrs (old: {
    # The Haskell environment does not come with cabal-install
    nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.cabal-install ];
  });

  # Shell with Haskell language server
  shell_hls = shell.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs
      ++ [ pkgs.haskellPackages.haskell-language-server ];
  });

  haskellPackages = pkgs.haskell.packages.ghc8104.override {
    overrides = self: super: {
      # used for asset loading
      wavefront = pkgs.haskell.lib.doJailbreak super.wavefront;
      # linear =
      #   pkgs.haskell.lib.appendBuildFlag super.linear "--ghc-options=-haddock";
      # JuicyPixels = pkgs.haskell.lib.appendBuildFlag super.JuicyPixels
      #   "--ghc-options=-haddock";
    };
  };

  app = ((haskellPackages.callCabal2nix "streamray" sources {
  }).overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs;
    passthru = oldAttrs.passthru // { inherit shell shell_hls; };
  })).override {
      random = haskellPackages.random_1_2_0;
  };

  formatting-fix = pkgs.mkShell {
    nativeBuildInputs = [ pkgs.haskellPackages.ormolu ];
    shellHook = ''
      shopt -s globstar
      ormolu --mode inplace **/*.hs
      exit 0
    '';
  };
}
