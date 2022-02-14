{ pkgs }:

let
    nixgl = (import ./sources.nix).nixgl {};
    # calibre needs to be wrapped with nixGL to run on non-NixOS distributions.
    # See https://github.com/NixOS/nixpkgs/issues/132045 for details.
    wrapperScript = ''
        #!${pkgs.runtimeShell}
        exec ${nixgl.auto.nixGLDefault}/bin/nixGL ${pkgs.calibre}/bin/calibre
    '';
in

pkgs.symlinkJoin {
  name = "calibre";
  paths = [ pkgs.calibre ];
  buildInputs = [ pkgs.makeWrapper ];
  postBuild = ''
      mv $out/bin/calibre $out/bin/calibre-wrapped
      echo "${wrapperScript}" > $out/bin/calibre
      chmod +x $out/bin/calibre
  '';
}
