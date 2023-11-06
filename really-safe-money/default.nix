{ mkDerivation, base, containers, deepseq, doctest, lib, validity
, validity-containers
}:
mkDerivation {
  pname = "really-safe-money";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq validity validity-containers
  ];
  testHaskellDepends = [ base doctest ];
  homepage = "https://github.com/NorfairKing/really-safe-money#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
