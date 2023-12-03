{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base, deepseq
, doctest, genvalidity-sydtest, lib, really-safe-money
, really-safe-money-gen, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "really-safe-money-autodocodec";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ autodocodec base really-safe-money ];
  testHaskellDepends = [
    aeson autodocodec autodocodec-yaml base deepseq doctest
    genvalidity-sydtest really-safe-money really-safe-money-gen sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/really-safe-money#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
