{ pkgs ? (import ./sources.nix).nixos-master {} }:

{
    ### Screenshots
    flameshot = pkgs.callPackage ./flameshot {};

    ### Ebooks
    knock = import ./knock;

    ### Text editors
    neovim = pkgs.callPackage ./nvim {};
    pyright = pkgs.pyright;
    vscode = pkgs.vscodium;

    ### Git
    git = pkgs.git;
    github-cli = pkgs.github-cli;

    ### AWS
    awscli2 = pkgs.awscli2;

    ### shell
    shellcheck = pkgs.shellcheck;

    ### Desktop
    xmonad = pkgs.callPackage ./xmonad {};
    volnoti = pkgs.callPackage ./volnoti.nix {};
}
