{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base, deepseq
, genvalidity-sydtest, lib, really-safe-money
, really-safe-money-autodocodec, really-safe-money-gen, sydtest
, sydtest-discover
}:
mkDerivation {
  pname = "really-safe-money-autodocodec-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base deepseq genvalidity-sydtest
    sydtest
  ];
  testHaskellDepends = [
    aeson base really-safe-money really-safe-money-autodocodec
    really-safe-money-gen sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/really-safe-money#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
