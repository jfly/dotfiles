{ pkgs }:

# TODO: simplify this once https://github.com/NixOS/nixpkgs/pull/162567 is merged up
let polybar_3_6_0 = pkgs.polybar.overrideAttrs (oldAttrs: rec {
    pname = "polybar";
    version = "3.6.0";

    # Override src to use the latest version of polybar.
    src = pkgs.fetchFromGitHub {
        owner = pname;
        repo = pname;
        rev = version;
        sha256 = "sha256-CgMXCo5YpnvuBNWSKZ8QdCcEb5bhnWx1DWBDy6fE1KI=";
        fetchSubmodules = true;
    };

    cmakeFlags = ["-DBUILD_CONFIG=off"];

    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.libuv ];
});

in
polybar_3_6_0.override {
    mpdSupport = true;
}
