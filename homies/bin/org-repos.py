#!/usr/bin/env python

import re
import sys
import argparse
import configparser
from pathlib import Path

def main():
    parser = argparse.ArgumentParser(description="Reorganize a directory of git repos under a ~/code/ directory.")
    parser.add_argument('--dry-run', action='store_true', help="do nothing, just print what would happen")
    args = parser.parse_args()

    if args.dry_run:
        print("Beginning a dry run.", file=sys.stderr)
        print()

    for p in Path(".").iterdir():
        analyze(p, dry_run=args.dry_run)

    if args.dry_run:
        print()
        print("Note: That was a dry run, nothing actually happened!", file=sys.stderr)

def find_remote(git_config: configparser.ConfigParser):
    remotes = [
        "upstream",
        "origin",
    ]
    for remote in remotes:
        section = f'remote "{remote}"'
        if section in git_config:
            return git_config[section]

    return None

def parse_remote(remote: str):
    regexps = [
        re.compile(r"git@(?P<service>github).com:(?P<org>.+)/(?P<repo>[^/]+).git"),
        re.compile(r"https://(?P<service>github).com/(?P<org>[^/]+)/(?P<repo>[^/]+)(.git)?"),
        re.compile(r"https://(?P<service>gitlab).(?P<instance>[^/]+)/(?P<org>[^/]+)/(?P<repo>[^/]+)(.git)?"),
    ]

    for regexp in regexps:
        if match := regexp.match(remote):
            service = match.group('service')
            instance = match.group('instance')
            org = match.group('org')
            repo = match.group('repo')
            return filter(None, [service, instance, org, repo])

    return None


def analyze(p: Path, dry_run: bool):
    if not p.is_dir():
        print(f"Skipping {p}: non-directory", file=sys.stderr)
        return

    types = [
        try_git,
        try_aur,
    ]

    fail_msgs = []
    for try_type in types:
        success, msg = try_type(p, dry_run)
        msg = f"{try_type.__name__}: {msg}"
        if success:
            print(msg, file=sys.stderr)
            return
        else:
            fail_msgs.append(msg)

    print(f"Couldn't figure out what to do with {p}. Errors:", file=sys.stderr)
    for fail_msg in fail_msgs:
        print(f"\t{fail_msg}")


def try_git(p: Path, dry_run: bool):
    git_config = p.joinpath(".git", "config")
    if not git_config.is_file():
        return False, f"Skipping {p}: Couldn't find {git_config}"

    config = configparser.ConfigParser()
    with git_config.open() as f:
        config.read_string(f.read())

    remote_section = find_remote(config)
    if remote_section is None:
        return False, f"Skipping {p}: git config has no recognized remotes"

    heirarchy = parse_remote(remote_section['url'])
    if heirarchy is None:
        return False, f"Skipping {p}: couldn't understand {remote_section['url']}"

    target = Path(Path.home(), "code").joinpath(*heirarchy)
    if not dry_run:
        target.parent.mkdir(parents=True, exist_ok=True)
        p.rename(target)

    return True, f"Renamed {p} -> {target}"


def try_aur(p: Path, dry_run: bool):
    pkgbuild = p.joinpath("PKGBUILD")
    if not pkgbuild.is_file():
        return False, f"Skipping {p}: Couldn't find {pkgbuild}"

    target = Path(Path.home(), "code", "aur", p.name)
    if not dry_run:
        target.parent.mkdir(parents=True, exist_ok=True)
        p.rename(target)

    return True, f"Renamed {p} -> {target}"

if __name__ == "__main__":
    main()
