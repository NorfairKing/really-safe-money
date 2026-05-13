{ haskellPackages, pkgs }:

let
  mutationCheck = pkgs.callPackage ./mutationCheck.nix { inherit haskellPackages; };
in
{
  mutation-really-safe-money = (mutationCheck {
    name = "really-safe-money";
    libraries = [
      "really-safe-money"
      "really-safe-money-autodocodec"
    ];
    tests = [
      "really-safe-money-gen"
    ];
  }).report;
}
