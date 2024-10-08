
{...}:

let
  nixpkgs-src = builtins.fetchTarball {
    # haskell-updates as of 2024-10-05
    url = "https://github.com/NixOS/nixpkgs/archive/edd71fd02c1f9a5dee102e2df2281327244c0119.tar.gz";
    sha256 = "03zs0ghm8kadgdpv9fgrppldnhxwsna3rcxy5wimjgs5y32jif6q";
  };

  my-overlay = import ./overlay.nix;

in

import nixpkgs-src { overlays = [ my-overlay ]; }
