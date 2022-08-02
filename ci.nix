{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
, pkgs ? import ./nix/pkgs.nix { inherit sources nixpkgs system; }
}:
let
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };

  versions = {
    "nixos-21_05" = sources.nixpkgs-21_05;
    "nixos-21_11" = sources.nixpkgs-21_11;
    "nixos-22_05" = sources.nixpkgs-22_05;
  };

  mkReleaseForVersion = version: nixpkgs:
    let
      p = import ./nix/pkgs.nix {
        inherit sources nixpkgs system;
      };

    in
    p.reallySafeMoneyRelease.overrideAttrs (old: { name = "really-safe-money-${version}"; });
in
{
  release = (import ./nix/pkgs.nix { inherit sources; }).reallySafeMoneyRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { }).run;
  hoogle = pkgs.buildEnv {
    name = "really-safe-money-hoogle";
    paths = [ (pkgs.haskellPackages.ghcWithHoogle (ps: pkgs.lib.attrValues pkgs.reallySafeMoneyPackages)) ];
  };
  shell = pkgs.symlinkJoin {
    name = "really-safe-money-shell";
    paths = (import ./shell.nix { inherit sources pkgs pre-commit; }).buildInputs;
  };
} // builtins.mapAttrs mkReleaseForVersion versions
