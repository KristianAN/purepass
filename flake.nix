{
  description = "my project description";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hPkgs =
          pkgs.haskell.packages."ghc964"; # need to match Stackage LTS version
                                           # from stack.yaml snapshot
        myDevTools = [
          pkgs.sqlite
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          hPkgs.ghcid # Continuous terminal Haskell compile checker
          hPkgs.ormolu # Haskell formatter
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          hPkgs.retrie # Haskell refactoring tool
          hPkgs.cabal-install
          pkgs.zlib # External C library needed by some Haskell packages
        ];

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
              in {
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };
      });
}

