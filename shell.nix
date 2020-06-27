{ sources ? import ./nix/sources.nix
}:
with { overlay = _: pkgs: { niv = import sources.niv {}; }; };
let
    pkgs = import sources.nixpkgs {};
    vimscript-haskell = import ./default.nix {};
in
  pkgs.haskellPackages.shellFor {
    packages = pkgs: [
      vimscript-haskell
    ];

    # withHoogle = true;
    buildInputs = with pkgs.haskellPackages; [
      cabal-install #  Make sure we are using cabal version 3
      ghcid
      pkgs.haskell.packages.ghc882.ghcide
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
