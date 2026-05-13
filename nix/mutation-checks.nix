{ haskellPackages }:

let
  inherit (haskellPackages.sydtest) mutationCheck;
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
  }).check;
}
