let
    pkgs = (import ./sources.nix).nixpkgs {};
    nixgl = (import ./sources.nix).nixgl {};
in

{
    ### Media
    #### Beets
    beets = pkgs.beets;
    abcde = pkgs.abcde;
    mp3val = pkgs.mp3val;
    # TODO: follow up after a while and see if we need these (plugins?) somehow.
    # AddPackage python-pyacoustid # Bindings for Chromaprint acoustic fingerprinting and the Acoustid API
    # AddPackage python-eyed3 # A Python module and program for processing information about mp3 files
    #### MPD
    ashuffle = pkgs.ashuffle;

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
    #### aliases::pdfcrop
    pdftk = pkgs.pdftk;
}
