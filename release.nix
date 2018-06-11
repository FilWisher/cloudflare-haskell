let
  pkgs = import <nixpkgs> { };
in
  { cloudflare-api = pkgs.haskellPackages.callPackage ./default.nix { };
  }
