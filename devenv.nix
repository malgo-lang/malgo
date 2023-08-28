{ pkgs, ... }:

{
  # https://devenv.sh/packages/
  packages = [ pkgs.git pkgs.llvmPackages_15.llvm pkgs.boehmgc ];

  # https://devenv.sh/scripts/
  # scripts.hello.exec = "echo hello from $GREET";

  enterShell = ''
    echo "llvm: " $(llvm-config --version)
    echo "libgc: " $(pkg-config --modversion bdw-gc)
    echo "git:" $(git --version)
  '';

  # https://devenv.sh/languages/
  # languages.nix.enable = true;
  languages.haskell.enable = true;
  languages.haskell.package = pkgs.haskell.compiler.ghc96;

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";

  # See full reference at https://devenv.sh/reference/options/
}
