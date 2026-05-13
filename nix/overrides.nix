{ lib
, haskell
, symlinkJoin
, ...
}:
with lib;
with haskell.lib;
self: super:
let
  reallySafeMoneyPackages =
    let
      reallySafeMoneyPkg = name:
        buildFromSdist (
          overrideCabal (self.callPackage ../${name} { })
            (old: {
              doBenchmark = true;
              doCheck = false; # Only in coverage report
              # cabal2nix maps 'license: AllRightsReserved' to
              # lib.licenses.unfree, which makes horizon-advance's nixpkgs
              # (no allowUnfree) refuse to evaluate. Override the Nix-level
              # license metadata so the forwardCompatibility check can
              # evaluate; the actual cabal license is unchanged.
              license = lib.licenses.bsd3;
              configureFlags = (old.configureFlags or [ ]) ++ [
                # Optimisations
                "--ghc-options=-O2"
                # Extra warnings
                "--ghc-options=-Wall"
                "--ghc-options=-Wincomplete-uni-patterns"
                "--ghc-options=-Wincomplete-record-updates"
                "--ghc-options=-Wpartial-fields"
                "--ghc-options=-Widentities"
                "--ghc-options=-Wredundant-constraints"
                "--ghc-options=-Wcpp-undef"
                "--ghc-options=-Werror"
              ];
            })
        );
    in
    genAttrs [
      "really-safe-money"
      "really-safe-money-autodocodec"
      "really-safe-money-autodocodec-gen"
      "really-safe-money-gen"
    ]
      reallySafeMoneyPkg;
in
reallySafeMoneyPackages // {
  inherit reallySafeMoneyPackages;
  reallySafeMoneyRelease = symlinkJoin {
    name = "really-safe-money-release";
    paths = attrValues self.reallySafeMoneyPackages;
  };
}
