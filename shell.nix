
let
  nixpkgsTarball = builtins.fetchTarball {
    # recent version of nixpkgs as of 2018-07-25
    url = "https://github.com/NixOS/nixpkgs/archive/4ccaa7de8eb34a0bb140f109a0e88095480118eb.tar.gz";
    sha256 = "0szbxfrzmlmxrgkqz5wnfgmsjp82vaddgz7mhdz7jj0jhd0hza4i";
  };
  nixpkgs = import nixpkgsTarball { };
in

with nixpkgs;

haskell.lib.buildStackProject {
  name = "xml-html-qq";
  buildInputs = [
    zlib
  ];
  ghc = haskell.compiler.ghc843;
}
