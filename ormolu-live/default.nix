let
  pkgs = import ../nix/pkgs.nix;
  hsPkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "ormolu-live";
      src = ../.;
      subDir = "ormolu-live";
    };
    compiler-nix-name = "ghc8107";
  };
  ormolu-live-js =
    hsPkgs.projectCross.ghcjs.hsPkgs.ormolu-live.components.exes.ormolu-live;
in {
  website = pkgs.stdenv.mkDerivation {
    name = "ormolu-live-website";
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "ormolu-live-www";
      src = ../.;
      subDir = "ormolu-live/www";
    };
    buildInputs = [ pkgs.closurecompiler ];
    installPhase = ''
      mkdir -p $out
      find . \( -name '*.html' -o -name '*.css' \) -exec cp {} $out \;
      ORMOLU_LIVE=${ormolu-live-js}/bin/ormolu-live.jsexe
      closure-compiler \
        $ORMOLU_LIVE/all.js --externs $ORMOLU_LIVE/all.js.externs \
        -O ADVANCED --jscomp_off=checkVars -W QUIET \
        --js_output_file $out/all.min.js
    '';
  };
  dev.shell = hsPkgs.shellFor {
    tools = { cabal = "latest"; };
    buildInputs = [ pkgs.git pkgs.ghcid ];
    withHoogle = false;
    exactDeps = false;
  };
}
