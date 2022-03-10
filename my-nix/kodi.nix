{ pkgs }:

let
    nixgl = (import ./sources.nix).nixgl {};
    # kodi needs to be wrapped with nixGL to run on non-NixOS distributions.
    wrapperScript = ''
        #!${pkgs.runtimeShell}
        exec ${nixgl.auto.nixGLDefault}/bin/nixGL ${pkgs.kodi}/bin/kodi
    '';
in

pkgs.symlinkJoin {
  name = "kodi";
  paths = [ pkgs.kodi ];
  buildInputs = [ pkgs.makeWrapper ];
  postBuild = ''
      rm $out/bin/kodi
      echo "${wrapperScript}" > $out/bin/kodi
      chmod +x $out/bin/kodi
  '';
}
