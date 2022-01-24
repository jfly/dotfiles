{ pkgs ? (import ./sources.nix).nixos-master {} }:

let symlink = target: name: pkgs.runCommand name {} ''
    mkdir -p $out/bin
    ln -s ${target} $out/bin/${name}
'';
in
{
    ### Screenshots
    flameshot = pkgs.callPackage ./flameshot {};

    ### Ebooks
    knock = import ./knock;

    ### Text editors
    neovim = pkgs.neovim;
    vi = symlink (pkgs.neovim + /bin/nvim) "vi";
    vim = symlink (pkgs.neovim + /bin/nvim) "vim";
}
