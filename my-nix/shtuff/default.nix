{ lib
, python3
, ps
, writeShellScriptBin
}:

let shtuff = with python3.pkgs; buildPythonApplication rec {
  pname = "shtuff";
  version = "0.3.2";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-hp4Ue3WzvOol/+ZU9vYhpMUY68TTl8ZMVbtcH8JjcGM=";
  };

  propagatedBuildInputs = [
    pexpect
    psutil
    pyxdg
    setproctitle
    setuptools
  ];

  checkPhase = ''
    # shtuff's tests start by trying to `pip install -e .`, which won't work:
    # pip will error out trying to write to some random, not-writeable part of
    # the nix store. Instead, direct pip to install shtuff somewhere else, and
    # update our PATH accordingly. This doesn't affec the final installation,
    # this is just necessary to get tests to run.
    export PIP_TARGET=./installed
    export PATH=$PATH:./installed
    make test
  '';

  postPatch = ''
    # shtuff uses `ps` internally. Point that to a direct path to ps.
    substituteInPlace shtuff.py \
      --replace "ps -p" "${ps}/bin/ps -p"
  '';

  meta = with lib; {
    inherit version;
    description = "It's like screen's stuff command, without screen";
    longDescription = ''
      Shell stuff will stuff commands into a shell à la tmux send-keys or screen stuff.
    '';
    homepage = "https://github.com/jfly/shtuff";
    license = licenses.mit;
  };
};
in

# Call the wrapped shtuff directly. The problem with the wrapped shtuff is that
# it sets the `PATH` (and less importantly, the `PYTHONNOUSERSITE`) environment
# variables, which impacts subprocesses in undesirable ways.
# For example, imagine you're in a direnv managed directory with a custom PATH.
# If you do something like `shtuff as work-project`, you likely will start a
# fresh shell
# (https://github.com/jfly/shtuff/blob/01ecf15875f06342456e29be19e9e3fbe63a60ae/shtuff.py#L91),
# but with your PATH prefixed by whatever nix's shtuff wrapper decided to put
# in there (relevant nix code here:
# https://github.com/NixOS/nixpkgs/blob/7682f18720f3cc0a0abfbb47e9e7612f83141f01/pkgs/development/interpreters/python/wrap.sh#L75)
# I don't know why nix needs the PATH env var set, because it already edited
# the `#!`s at the top of the relevant files to point at the correct python.
# The `PYTHONNOUSERSITE` is important (to make sure we don't import some random
# version of shtuff we `pip install --local`-ed at some point, but that can be
# replicated with python's `-s` parameter. I guess the only weirdness here
# would be if shtuff itself decided to try to invoke python as a subprocess,
# but that feels like a weird thing to do, and I know shtuff doesn't do that.
# TODO: seek help upstream with nix to see if there's a more appropriate way of
#       accomplishing this.
writeShellScriptBin "shtuff" ''
  ${python3}/bin/python -s ${shtuff}/bin/.shtuff-wrapped "$@"
''
