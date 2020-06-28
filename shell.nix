{ sources ? import ./nix/sources.nix
}:
with { overlay = _: pkgs: { niv = import sources.niv {}; }; };
let
    pkgs = import sources.nixpkgs {};
    vimscript-haskell = import ./default.nix {};

    # Commands
    commands = {
      test-watch = pkgs.writeShellScriptBin "test-watch" ''
        ls src/*.hs test/*.hs | entr -dsc 'cabal build && cabal run vimscript-test'
        '';

      run-watch = pkgs.writeShellScriptBin "run-watch" ''
        ls src/*.hs | entr -dsc 'cabal build && cabal run vimscript-haskell-exe'
        '';
    };
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
      pkgs.entr
      pkgs.vim
      commands.test-watch
      commands.run-watch
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
