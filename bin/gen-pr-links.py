#!/usr/bin/env python

import subprocess


def get_branchname(ref):
    ref, branchname = (
        subprocess.check_output(['git', 'name-rev', ref], encoding='UTF-8')
        .strip()
        .split(" ")
    )
    return branchname


def generate_branch_pairs():
    pairs = []
    i = 0
    while True:
        branch_ref = "HEAD" + "^" * i
        branch = get_branchname(branch_ref)
        parent_branch = get_branchname(branch_ref + "^")
        pairs.append((parent_branch, branch))
        if "upstream/master" in parent_branch:
            break

        i += 1

    return pairs


def generate_ghpr_link(parent_branch, branch):
    target_repo = "joinhonor"
    if "remotes/upstream" in parent_branch:
        target_repo = "joinhonor"
        parent_branch = parent_branch[len("remotes/upstream/") :]

    return f"https://github.com/{target_repo}/external-api/compare/{parent_branch}...joinhonor:{branch}?expand=1"


def main():
    print("\n".join([generate_ghpr_link(*pair) for pair in generate_branch_pairs()]))


if __name__ == "__main__":
    main()
