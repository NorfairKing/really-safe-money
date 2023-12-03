{ mkDerivation, base, containers, deepseq, doctest, lib, scientific
, validity, validity-containers, validity-scientific
}:
mkDerivation {
  pname = "really-safe-money";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq scientific validity validity-containers
    validity-scientific
  ];
  testHaskellDepends = [ base doctest ];
  homepage = "https://github.com/NorfairKing/really-safe-money#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
