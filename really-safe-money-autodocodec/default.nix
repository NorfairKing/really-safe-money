{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base, deepseq
, genvalidity, genvalidity-sydtest, lib, really-safe-money
, really-safe-money-gen, safe-coloured-text, sydtest
, sydtest-discover, text
}:
mkDerivation {
  pname = "really-safe-money-autodocodec";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    autodocodec base genvalidity really-safe-money text
  ];
  testHaskellDepends = [
    aeson autodocodec autodocodec-yaml base deepseq genvalidity-sydtest
    really-safe-money really-safe-money-gen safe-coloured-text sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/really-safe-money#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
