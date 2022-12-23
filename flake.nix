{
  description = "malgo flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc925;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "malgo";

        llvm-hs-repo = pkgs.fetchFromGitHub {
          owner = "MuKnIO";
          repo = "llvm-hs";
          rev = "fe8fd556e8d2cc028f61d4d7b4b6bf18c456d090";
          sha256 = "1b4kh0c8sgb39qzfi3cnagz1ssgl96mzb6s18xapy0qj23zql0p0";
        };
        llvm-hs = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.overrideCabal ((haskellPackages.callCabal2nix "llvm-hs" "${llvm-hs-repo}/llvm-hs" {
          inherit llvm-hs-pure;
        }).overrideAttrs (oldAttrs: rec {
          buildInputs = oldAttrs.buildInputs ++ [
            pkgs.llvm_12
          ];
        })) (oldAttrs: {
          preCompileBuildDriver = oldAttrs.preCompileBuildDriver or "" + ''
            substituteInPlace Setup.hs --replace "addToLdLibraryPath libDir" "pure ()"
          '';
        }));
        llvm-hs-pure = haskellPackages.callCabal2nix "llvm-hs-pure" "${llvm-hs-repo}/llvm-hs-pure" {};
      in {
        packages.${packageName} = # (ref:haskell-package-def)
          pkgs.haskell.lib.addTestToolDepends
            (pkgs.haskell.lib.addBuildDepends
              (haskellPackages.callCabal2nix packageName self rec {
                # Dependency overrides go here
                inherit llvm-hs;
                inherit llvm-hs-pure;
                generic-data = pkgs.haskell.lib.dontCheck haskellPackages.generic-data;
                diagnose = (pkgs.haskell.lib.overrideCabal haskellPackages.diagnose (drv: {
                  configureFlags = ["-fmegaparsec-compat" ];
                  buildDepends = (drv.buildDepends or []) ++ [haskellPackages.megaparsec];
                }));
              }) [ pkgs.boehmgc ])
            [ pkgs.which pkgs.pkg-config ];
        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}

