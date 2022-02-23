{
  nixpkgs = import (builtins.fetchTarball {
    name = "nixpkgs-unstable";
    url = "https://github.com/NixOS/nixpkgs/archive/74b10859829153d5c5d50f7c77b86763759e8654.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0g9gak16a0mx6kwjzpz8fx4rwl9p1jj8f4f4frl12vjhnrssf6zp";
  });
  nixgl = import (builtins.fetchTarball {
    name = "nixGL";
    url = "https://github.com/guibou/nixGL/archive/17658df1e17a64bc23ee5c93cfa9e8b663a4ac81.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "18adz8bli9gq619mm8y7m8irjbh9dg0mg31wrrcrky7w3al8g7ph";
  });
}
