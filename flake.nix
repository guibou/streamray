{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:

    let pkgs = nixpkgs.legacyPackages.${system};
    in
    rec {
      devShells = rec {
        shell = (packages.default.envFunc { withHoogle = false; }).overrideAttrs (old: {
          # The Haskell environment does not come with cabal-install
          nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.cabal-install ];
        });

        # Shell with Haskell language server
        shell_hls = shell.overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs
            ++ [ pkgs.haskellPackages.haskell-language-server ];
        });

        default = shell_hls;
      };

      packages = rec {
          haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: {
              #PyF = pkgs.haskell.lib.unmarkBroken super.PyF;
              # used for asset loading
              wavefront = pkgs.haskell.lib.unmarkBroken (pkgs.haskell.lib.doJailbreak super.wavefront);
              # linear =
              #   pkgs.haskell.lib.appendBuildFlag super.linear "--ghc-options=-haddock";
              # JuicyPixels = pkgs.haskell.lib.appendBuildFlag super.JuicyPixels
              #   "--ghc-options=-haddock";
            };
          };

          default = ((haskellPackages.callCabal2nix "streamray" ./. {
          }).overrideAttrs (oldAttrs: {
            buildInputs = oldAttrs.buildInputs;
          })).override {
              #random = haskellPackages.random_1_2_0;
              #linear = pkgs.haskell.lib.dontCheck (haskellPackages.linear.override {
              #  random = haskellPackages.random_1_2_0;
              #});
          };
      };
    }
  );
}
