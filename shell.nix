{ sources ? import ./nix/sources.nix
}:
with { overlay = _: pkgs: { niv = import sources.niv {}; }; };
let
    pkgs = import sources.nixpkgs {};
    vimscript-haskell = import ./default.nix {};

    # Commands
    commands = {
      test-watch = pkgs.writeShellScriptBin "test-watch" ''
        find src test -name '*.hs' | entr -dsc 'cabal build && cabal run vimscript-test'
        sleep 1
        '';

      run-watch = pkgs.writeShellScriptBin "run-watch" ''
        find src -name '*.hs' | entr -dsc 'cabal build && cabal run vimscript-haskell-exe'
        sleep 1
        '';

      run = pkgs.writeShellScriptBin "run" ''
        cabal build && cabal run vimscript-haskell-exe
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
      commands.run
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
