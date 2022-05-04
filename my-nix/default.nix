let
    pkgs = (import ./sources.nix).nixpkgs {};
    wrapNixGL = pkgs.callPackage ./wrap-nixgl.nix {};
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
    # calibre needs to be wrapped with nixGL to run on non-NixOS distributions.
    # See https://github.com/NixOS/nixpkgs/issues/132045 for details.
    calibre = wrapNixGL pkgs.calibre;
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
    shfmt = pkgs.shfmt;

    ### Desktop
    xmonad = pkgs.callPackage ./xmonad {};
    volnoti = pkgs.callPackage ./volnoti.nix {};
    polybar = pkgs.callPackage ./polybar.nix {};
    # kodi needs to be wrapped with nixGL to run on non-NixOS distributions.
    kodi = wrapNixGL (pkgs.callPackage ./kodi {});
    with-alacritty = wrapNixGL (pkgs.callPackage ./with-alacritty {});

    ### Development
    xxd = pkgs.xxd;
    rsync = pkgs.rsync;
    mycli = pkgs.callPackage ./mycli {};
    shtuff = pkgs.callPackage ./shtuff {};
    yq = pkgs.yq;

    ### Virtualization
    vagrant = pkgs.vagrant.override {
        # Turn off libvirt. I don't need it, and if it's present, it causes
        # vagrant to crash with the following:
        #
        #  > Vagrant failed to initialize at a very early stage:
        #  >
        #  > The plugins failed to load properly. The error message given is
        #  > shown below.
        #  >
        #  > cannot load such file -- vagrant/libvirt
        withLibvirt=false;
    };

    ### Debug utils
    strace = pkgs.strace;

    ### Homies
    #### aliases::pdfcrop
    pdftk = pkgs.pdftk;

    ### bin scripts
    paste-list = (pkgs.callPackage ./paste-list {}).paste-list;
}
