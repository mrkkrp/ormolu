let
  pkgs = import ../nix/pkgs.nix;
  sources = import ../nix/sources.nix { };
  easy-ps = import sources.easyPurescript { inherit pkgs; };
in
pkgs.mkShell {
  name = "ormolu-live";
  buildInputs = [
    pkgs.nodejs
    easy-ps.purescript
    easy-ps.spago
    easy-ps.purescript-language-server
    easy-ps.purty
    easy-ps.zephyr
  ];
}
