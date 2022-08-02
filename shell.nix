{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, pkgs ? import ./nix/pkgs.nix { inherit nixpkgs sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "really-safe-money-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook;
}
