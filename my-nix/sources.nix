{
  nixpkgs = import (builtins.fetchTarball {
    name = "nixpkgs-unstable";
    url = "https://github.com/NixOS/nixpkgs/archive/1d08ea2bd83abef174fb43cbfb8a856b8ef2ce26.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1q8p2bz7i620ilnmnnyj9hgx71rd2j6sjza0s0w1wibzr9bx0z05";
  });
  nixgl = import (builtins.fetchTarball {
    name = "nixGL";
    url = "https://github.com/guibou/nixGL/archive/17658df1e17a64bc23ee5c93cfa9e8b663a4ac81.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "18adz8bli9gq619mm8y7m8irjbh9dg0mg31wrrcrky7w3al8g7ph";
  });
}
