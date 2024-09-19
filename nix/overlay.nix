final: prev: {

  ###############################
  ## Haskell package overrides ##
  ###############################

  myhaskell =
    let
      myHaskellOverride = oldAttrs: {
        overrides =
          final.lib.composeExtensions
            (oldAttrs.overrides or (_: _: {}))
            (hfinal: hprev: {
              shebanger =
                let
                  filesToIgnore = [
                    ".git"
                    ".github"
                    ".stack-work"
                    ".travis.yml"
                    "cabal.project"
                    "default.nix"
                    "flake.lock"
                    "flake.nix"
                    "nix"
                    "result"
                    "shell.nix"
                    "stack-nightly.yaml"
                    "stack.yaml"
                  ];

                  src =
                    builtins.path {
                      name = "shebanger-src";
                      path = ./..;
                      filter = path: type:
                        with final.lib;
                        ! elem (baseNameOf path) filesToIgnore &&
                        ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ];
                    };

                  extraCabal2nixOptions = "";
                in
                hfinal.callCabal2nixWithOptions "shebanger" src extraCabal2nixOptions {};
            });
      };
    in
    prev.haskell // {
      packages = prev.haskell.packages // {
        ${final.shebanger-ghc-version} =
          prev.haskell.packages.${final.shebanger-ghc-version}.override
            myHaskellOverride;
      };
    };

  shebanger-ghc-version-short = "966";

  shebanger-ghc-version = "ghc" + final.shebanger-ghc-version-short;

  shebanger-haskell-pkg-set = final.myhaskell.packages.${final.shebanger-ghc-version};

  shebanger = final.shebanger-haskell-pkg-set.shebanger;

  shebanger-just-exe =
    final.lib.pipe
      final.shebanger
      [
        (final.shebanger-haskell-pkg-set.generateOptparseApplicativeCompletions ["shebanger"])
        final.haskell.lib.justStaticExecutables
      ];

  shebanger-shell = final.shebanger-haskell-pkg-set.shellFor {
    packages = hpkgs: [ hpkgs.shebanger ];
  };

  my-haskell-language-server =
    final.haskell-language-server.override {
      supportedGhcVersions = [ final.shebanger-ghc-version-short ];
    };

  dev-shell = final.mkShell {
    nativeBuildInputs = [
      final.cabal-install
      final.my-haskell-language-server
    ];
    inputsFrom = [
      final.shebanger-shell
    ];
    # These environment variables are important. Without these,
    # doctest doesn't pick up nix's version of ghc, and will fail
    # claiming it can't find your dependencies
    shellHook = ''
      export NIX_GHC="${final.shebanger-shell.NIX_GHC}"
      export NIX_GHC_LIBDIR="${final.shebanger-shell.NIX_GHC_LIBDIR}"
    '';
  };
}

