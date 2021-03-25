{ pkgs ? import ./nixpkgs.nix { } }: rec {
  inherit pkgs;

  # Explicit list of used files. Else there is always too much and
  # cache is invalidated.
  sources = pkgs.lib.sourceByRegex ./. [
    ".*\.cabal$"
    ".*\.hs$"
    ".*\.md$"
    "src"
    "app"
    "tests"
    "LICENSE"
  ];

  shell = (app.envFunc { withHoogle = true;}).overrideAttrs (old: {
    # The Haskell environment does not come with cabal-install
    nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.cabal-install ];
  });

  # Shell with Haskell language server
  shell_hls = shell.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs
      ++ [ pkgs.haskellPackages.haskell-language-server ];
  });

  app = (pkgs.haskell.lib.buildFromSdist ((pkgs.haskellPackages.override {
    overrides = self: super: {
      wavefront = pkgs.haskell.lib.doJailbreak super.wavefront;
    };
  }).callCabal2nix "streamray" sources { })).overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs;
    passthru = oldAttrs.passthru // { inherit shell shell_hls; };
  });

  formatting-fix = pkgs.mkShell {
    nativeBuildInputs =
      [ pkgs.haskellPackages.ormolu ];
    shellHook = ''
      ormolu --mode inplace **/*.hs
      exit 0
    '';
  };
}
