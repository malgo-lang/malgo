final: prev: {
  mkDerivation = args: prev.mkDerivation (args // {
    doHaddock = false;
    doCheck = false;
  });
}
