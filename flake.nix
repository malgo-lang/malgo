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

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "malgo";

        llvm-hs-repo = pkgs.fetchFromGitHub {
          owner = "MuKnIO";
          repo = "llvm-hs";
          rev = "fe8fd556e8d2cc028f61d4d7b4b6bf18c456d090";
          sha256 = "1b4kh0c8sgb39qzfi3cnagz1ssgl96mzb6s18xapy0qj23zql0p0";
        };
        llvm-hs = (haskellPackages.callCabal2nix "llvm-hs" "${llvm-hs-repo}/llvm-hs" {
          inherit llvm-hs-pure;
        }).overrideAttrs (oldAttrs: rec {
          buildInputs = oldAttrs.buildInputs ++ [
            pkgs.llvm_12
          ];
        });
        llvm-hs-pure = haskellPackages.callCabal2nix "llvm-hs-pure" "${llvm-hs-repo}/llvm-hs-pure" {};
      in {
        packages.${packageName} = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
            inherit llvm-hs;
            inherit llvm-hs-pure;
          };

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

