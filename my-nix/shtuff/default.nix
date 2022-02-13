{ lib
, python3
, ps
}:

with python3.pkgs;

buildPythonApplication rec {
  pname = "shtuff";
  version = "0.3.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-99TyWqTOu9vxKaJUFFTr+dyHDbo+wDrGuQsGuujmwHA=";
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
}
