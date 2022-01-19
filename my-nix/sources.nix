{
  nixos-master = import (builtins.fetchTarball {
    name = "nixos-master";
    url = "https://github.com/NixOS/nixpkgs/archive/7682f18720f3cc0a0abfbb47e9e7612f83141f01.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1qnrmsc9lik98zqhx7qxdxhmvbb8279x6win0mvmdmcmk9wbqznh";
  });
}
