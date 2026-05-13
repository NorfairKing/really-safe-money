{ haskellPackages, pkgs }:

let
  mutationCheck = pkgs.callPackage ./mutationCheck.nix { inherit haskellPackages; };
  assertMutationScore = haskellPackages.sydtest.assertMutationScore;
  reallySafeMoneyReport = (mutationCheck {
    name = "really-safe-money";
    libraries = [
      "really-safe-money"
      "really-safe-money-autodocodec"
    ];
    tests = [
      "really-safe-money-gen"
    ];
  }).report;
in
{
  mutation-really-safe-money = assertMutationScore {
    name = "mutation-really-safe-money";
    report = reallySafeMoneyReport;
  };
}
