{ pkgs }:

pkgs.neovim.override {
    vimAlias = true;
    viAlias = true;
    configure = {
        packages.myPackages = with pkgs.vimPlugins; {
            start = [
                fzf-vim
                MatchTagAlways
                vim-airline
                matchit-zip
                tcomment_vim
                vim-colors-solarized
                vim-airline-themes
                vim-fugitive
                vim-rhubarb
                vim-polyglot
                vim-test
                traces-vim
                vim-rsi # readline shortcuts in useful places
                # TODO: wait a while and if you actually miss these
                # neoterm
                # tagbar
                # codi-vim
                vim-mergetool
                (pkgs.vimUtils.buildVimPlugin {
                    pname = "honorjs-test-runner";
                    version = "0.0.1";
                    src = ./plugin;
                })

                # Linting/autofixing/LSP, etc
                editorconfig-vim
                ale
                nvim-lspconfig
                cmp-nvim-lsp
                cmp-buffer
                nvim-cmp
                null-ls-nvim
            ];
        };
        customRC = builtins.readFile ./vimrc;
    };
}
