final: prev: {
  mkDerivation = args: prev.mkDerivation (args // {
    enableLibraryProfiling = false;
    # doHaddock = false;
    doCheck = false;
  });
}
