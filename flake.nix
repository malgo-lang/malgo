{
  description = "Malgo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc910.override {
          overrides = import ./nix/haskell-overrides.nix pkgs;
        };

        jailbreakUnbreak =
          pkg:
          pkgs.haskell.lib.doJailbreak (
            pkg.overrideAttrs (_: {
              meta = { };
            })
          );

        packageName = "malgo";
      in
      {
        packages.${packageName} = pkgs.haskell.lib.doCheck (
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          }
        );

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
            haskellPackages.hpack
            nixfmt-rfc-style
            entr
            fd
            haskellPackages.hoogle
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      }
    );
}
