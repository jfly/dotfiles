import argparse
import logging
import shlex
import json
import sys

from . import core


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--verbose", "-v", action="count", default=0)
    subparsers = parser.add_subparsers(required=True)

    parser_alacritty = subparsers.add_parser(
        "alacritty", help="invoke alacritty", add_help=False
    )
    parser_alacritty.set_defaults(func=do_alacritty, allow_unknown_args=True)

    which_help = "Which terminal(s) to affect. Choices are: default, global, current, or PID. 'current' only works there's an ancestor process named 'alacritty'."

    parser_get = subparsers.add_parser(
        "get",
        help="get config values",
        description="Get settings for given terminal, or all terminals.",
    )
    parser_get.add_argument("which", help=which_help)
    parser_get.add_argument("--pretty", action="store_true")
    parser_get.set_defaults(func=do_get, allow_unknown_args=False)

    parser_set = subparsers.add_parser(
        "set",
        help="set config values",
        description="Set settings for given terminal, or all terminals. Unlike update, this clobbers the existing config rather than merges with it.",
    )
    parser_set.add_argument("which", help=which_help)
    parser_set.add_argument("value", help="JSON object to clobber existing config with")
    parser_set.set_defaults(func=do_set, allow_unknown_args=False)

    parser_update = subparsers.add_parser(
        "update",
        help="update config values",
        description="Update settings for given terminal, or all terminals. Unlike set, this merges with the existing config.",
    )
    parser_update.add_argument("which", help=which_help)
    parser_update.add_argument(
        "value", help="JSON object to merge with the existing config"
    )
    parser_update.set_defaults(func=do_update, allow_unknown_args=False)

    parser_whoami = subparsers.add_parser(
        "whoami",
        description="Prints out the PID of the nearest ancestor process named 'alacritty'. This value can be used when calling '%(prog)s set'",
    )
    parser_whoami.set_defaults(func=do_whoami, allow_unknown_args=True)

    args, rest = parser.parse_known_args()
    if not args.allow_unknown_args and rest:
        print(f"error: unrecognized arguments: {shlex.join(rest)}")
        sys.exit(1)

    logging.basicConfig(level=verbosity_to_log_level(args.verbose))
    args.func(args, rest)


def verbosity_to_log_level(verbosity: int) -> int:
    if verbosity == 0:
        return logging.WARNING
    else:
        return logging.DEBUG


def do_alacritty(_args: argparse.Namespace, rest: list[str]):
    core.start_new_terminal(rest)


def parse_which(raw_which: str) -> core.Which:
    if raw_which == "default":
        which = "default"
    elif raw_which == "global":
        which = "global"
    elif raw_which == "current":
        which = "current"
    else:
        pid = int(raw_which)
        which = pid
    return which


def do_get(args: argparse.Namespace, _rest: list[str]):
    which = parse_which(args.which)
    indent = 4 if args.pretty else None
    json.dump(core.get(which), indent=indent, fp=sys.stdout)
    print()


def do_update(args: argparse.Namespace, _rest: list[str]):
    which = parse_which(args.which)
    new_settings = json.loads(args.value)
    core.update(which, new_settings, merge_with_existing=True)


def do_set(args: argparse.Namespace, _rest: list[str]):
    which = parse_which(args.which)
    new_settings = json.loads(args.value)
    core.update(which, new_settings, merge_with_existing=False)


def do_whoami(_args: argparse.Namespace, _rest: list[str]):
    print(core.get_current_terminal_pid())


if __name__ == "__main__":
    main()
