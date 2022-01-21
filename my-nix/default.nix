{ pkgs ? (import ./sources.nix).nixos-master {} }:

{
  flameshot = pkgs.callPackage ./flameshot {};
  knock = import ./knock;
}
