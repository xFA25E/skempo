let
  pkgs = import <nixpkgs> {};
  eldev = pkgs.stdenv.mkDerivation rec {
    pname = "eldev";
    version = "1.0";
    src = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/doublep/eldev/${version}/bin/eldev";
      sha256 = "0spni3il3myv70zk18k76sj6yvh3rqi9j9wglz2j5mfs7z2dnr5f";
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
