let
  pkgs = import <nixpkgs> {};
  eldev = pkgs.stdenv.mkDerivation rec {
    pname = "eldev";
    version = "0.9.3";
    src = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/doublep/eldev/${version}/bin/eldev";
      sha256 = "0ikhhfxm1rz3wp37spsy8bcnx5071ard71pd1riw09rsybilxhgn";
    };
    nativeBuildInputs = [ pkgs.makeWrapper ];
    unpackPhase = "true";
    installPhase = ''
      install -D -v -m555 "$src" "$out/bin/eldev"
      wrapProgram "$out/bin/eldev" --set ELDEV_EMACS "${pkgs.emacs}/bin/emacs"
    '';
  };
in pkgs.mkShell {
  name = "skempo-dev-env";
  buildInputs = [ eldev ];
  shellHook = ''
    export ELDEV_DIR=$PWD/.eldev
  '';
}
