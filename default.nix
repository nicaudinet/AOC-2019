{ pkgs ? import <nixpkgs> {} }:

let

  day1 = pkgs.haskellPackages.callCabal2nix "day1" ./day1/. {};
  day2 = pkgs.haskellPackages.callCabal2nix "day2" ./day2/. {};

  allBuildInputs =
    day1.buildInputs ++ day2.buildInputs;

  allPropagatedBuildInputs =
    day1.propagatedBuildInputs ++ day2.propagatedBuildInputs;

in
  
{
  inherit day1 day2;
  shell = pkgs.buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with pkgs.haskellPackages; [
      ghcid
      cabal-install
      (ghcWithPackages (p: allBuildInputs ++ allPropagatedBuildInputs))
    ];
  };
}
