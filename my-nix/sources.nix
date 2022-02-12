{
  nixos-master = import (builtins.fetchTarball {
    name = "nixos-master";
    url = "https://github.com/NixOS/nixpkgs/archive/1882c6b7368fd284ad01b0a5b5601ef136321292.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0zg7ak2mcmwzi2kg29g4v9fvbvs0viykjsg2pwaphm1fi13s7s0i";
  });
}
