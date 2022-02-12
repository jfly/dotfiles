{ pkgs }:

pkgs.flameshot.overrideAttrs (oldAttrs: {
  patches = [
    ./0000-issue-1072-workaround.diff
    ./0001-issue-2283-workaround.diff
  ];
})
