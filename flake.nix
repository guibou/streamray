{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    PyF.url = "github:guibou/PyF";
  };

  outputs = {nixpkgs, flake-utils, PyF, ...}:
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
          makePkgSet = hpkgs: hpkgs.override {
            overrides = self: super: with pkgs.haskell.lib; {
              # used for asset loading
              wavefront = unmarkBroken (doJailbreak super.wavefront);
              JuicyPixels = doJailbreak super.JuicyPixels;
              PyF = super.callCabal2nix "PyF" PyF {};
              binary-orphans = doJailbreak super.binary-orphans;
              lens = doJailbreak super.lens;
            };
          };

          streamray_ghc92 = (makePkgSet pkgs.haskellPackages).callCabal2nix "streamray" ./. {};
          streamray_ghc96 = (makePkgSet pkgs.haskell.packages.ghc961).callCabal2nix "streamray" ./. {};
          default = streamray_ghc92;
      };
    }
  );
}
