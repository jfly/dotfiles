{ lib, python3Packages }:
with python3Packages; buildPythonApplication {
  pname = "with-alacritty";
  version = "1.0";

  propagatedBuildInputs = [
      mergedeep
      psutil
      pyxdg
  ];

  nativeBuildInputs = [
      black
  ];

  src = ./.;
}
