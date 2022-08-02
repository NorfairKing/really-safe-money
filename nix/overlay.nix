final:
previous:
with final.haskell.lib;
{
  reallySafeMoneyPackages =
    let
      reallySafeMoneyPkg = name:
        buildFromSdist (
          overrideCabal (final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { })
            (old: {
              doBenchmark = true;
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
                "--ghc-options=-Wno-deprecations"
              ];
              # Ugly hack because we can't just add flags to the 'test' invocation.
              # Show test output as we go, instead of all at once afterwards.
              testTarget = (old.testTarget or "") + " --show-details=direct";
            })
        );
    in
    final.lib.genAttrs [
      "really-safe-money"
      "really-safe-money-gen"
    ]
      reallySafeMoneyPkg;

  reallySafeMoneyRelease =
    final.symlinkJoin {
      name = "really-safe-money-release";
      paths = final.lib.attrValues final.reallySafeMoneyPackages;
    };

  haskellPackages = previous.haskellPackages.override (
    old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
        self: super: final.reallySafeMoneyPackages
      );
    }
  );
}
