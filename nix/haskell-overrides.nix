pkgs: final: prev: {
  mkDerivation = args: prev.mkDerivation (args // {
    enableLibraryProfiling = false;
    # doHaddock = false;
    doCheck = false;
  });
  lens = final.callHackageDirect {
    pkg = "lens";
    ver = "5.3.4";
    sha256 = "sha256-g6pzTgFe1+bAAwRUjyHGbVZ8jGc1/01HbAGZEuqktZ8=";
  } {};
}
