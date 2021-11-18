import re

__all__ = ['parse_remote']

def parse_remote(remote: str):
    regexps = [
        re.compile(r"^git@(?P<service>[^/]+):(?P<org>.+)/(?P<repo>[^/]+).git$"),
        re.compile(r"^https://(?P<service>[^/]+)/(?P<org>[^/]+)/(?P<repo>[^/]+?)(.git)?$"),
    ]

    for regexp in regexps:
        if match := regexp.match(remote):
            service = match.group('service')
            org = match.group('org')
            repo = match.group('repo')
            return [service, org, repo]

    return None
