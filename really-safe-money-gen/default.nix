{ mkDerivation, base, criterion, deepseq, genvalidity
, genvalidity-containers, genvalidity-criterion
, genvalidity-sydtest, genvalidity-vector, lib, QuickCheck
, really-safe-money, sydtest, sydtest-discover, vector
}:
mkDerivation {
  pname = "really-safe-money-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-containers QuickCheck
    really-safe-money
  ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-vector really-safe-money
    sydtest vector
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion deepseq genvalidity genvalidity-criterion
    genvalidity-vector QuickCheck really-safe-money vector
  ];
  homepage = "https://github.com/NorfairKing/really-safe-money#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
