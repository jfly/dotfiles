{ pkgs }:

# This was largely copied from nixos/modules/services/x11/window-managers/xmonad.nix
let
    haskellPkgs = pkgs.haskellPackages;
    xmonadAndPackages = [haskellPkgs.xmonad_0_17_0 haskellPkgs.xmonad-contrib_0_17_0];
    xmonadEnv = haskellPkgs.ghcWithPackages (p: xmonadAndPackages);
    configured = pkgs.writers.writeHaskellBin "xmonad" {
        libraries = xmonadAndPackages;
    } ./xmonad.hs;
in

pkgs.runCommandLocal "xmonad" {
    nativeBuildInputs = [ pkgs.makeWrapper ];
} ''
    install -D ${xmonadEnv}/share/man/man1/xmonad.1.gz $out/share/man/man1/xmonad.1.gz
    makeWrapper ${configured}/bin/xmonad $out/bin/xmonad \
        --set XMONAD_XMESSAGE "${pkgs.xorg.xmessage}/bin/xmessage"
''
