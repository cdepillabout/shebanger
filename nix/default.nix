
{...}:

let
  nixpkgs-src = builtins.fetchTarball {
    # nixpkgs-unstable as of 2024-08-04
    url = "https://github.com/NixOS/nixpkgs/archive/81610abc161d4021b29199aa464d6a1a521e0cc9.tar.gz";
    sha256 = "19j550srrsmsfzz0arfva1n13kjdz5yiz3x2ss3mgpaxacny7iad";

    # # nixos-24.05 as of 2024-07-08
    # url = "https://github.com/NixOS/nixpkgs/archive/49ee0e94463abada1de470c9c07bfc12b36dcf40.tar.gz";
    # sha256 = "142yikglqm22yzn4m6ccwkf55rqyrn94fr6qmf9dsmfcag8dbc2s";

    # nixos-23.11 as of 2024-07-08
    # url = "https://github.com/NixOS/nixpkgs/archive/7144d6241f02d171d25fba3edeaf15e0f2592105.tar.gz";
    # sha256 = "1lm7rkcbr7gg5zp62bga8iqyhg5hsvcly95hq0p3mcv7zq8n3wc2";
  };

  my-overlay = import ./overlay.nix;

in

import nixpkgs-src { overlays = [ my-overlay ]; }
