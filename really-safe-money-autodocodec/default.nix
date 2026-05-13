{ mkDerivation, autodocodec, base, doctest, lib, really-safe-money
}:
mkDerivation {
  pname = "really-safe-money-autodocodec";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ autodocodec base really-safe-money ];
  testHaskellDepends = [ base doctest ];
  homepage = "https://github.com/NorfairKing/really-safe-money#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
