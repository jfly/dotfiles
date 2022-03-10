{
  nixpkgs = import (builtins.fetchTarball {
    name = "nixpkgs-unstable";
    url = "https://github.com/NixOS/nixpkgs/archive/639d0ff3523f1b4b0f82cf51dba4697b9c89323b.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1a70hh19v3x64yry97akkdmdjb6xf7h255vl4vzdm6ifaz4arb1g";
  });
  nixgl = import (builtins.fetchTarball {
    name = "nixGL";
    url = "https://github.com/guibou/nixGL/archive/17658df1e17a64bc23ee5c93cfa9e8b663a4ac81.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "18adz8bli9gq619mm8y7m8irjbh9dg0mg31wrrcrky7w3al8g7ph";
  });
}
