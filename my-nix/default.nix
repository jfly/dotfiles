let
    pkgs = (import ./sources.nix).nixpkgs {};
    nixgl = (import ./sources.nix).nixgl {};
in

{
    ### Screenshots
    flameshot = pkgs.callPackage ./flameshot {};

    ### Ebooks
    calibre = pkgs.callPackage ./calibre.nix {};
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

    ### Development
    mycli = pkgs.callPackage ./mycli {};
    shtuff = pkgs.callPackage ./shtuff {};

    ### Debug utils
    ghidra = pkgs.ghidra-bin;
    strace = pkgs.strace;

    ### Homies
    ## aliases::pdfcrop
    pdftk = pkgs.pdftk;
}
