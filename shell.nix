with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "skempo-dev-env";
  buildInputs = [ eldev ];
}
