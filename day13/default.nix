{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "day13" ./. {}
