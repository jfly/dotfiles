import re 

__all__ = ['parse_remote']

def parse_remote(remote: str):
    regexps = [
        re.compile(r"^git@(?P<service>github).com:(?P<org>.+)/(?P<repo>[^/]+).git$"),
        re.compile(r"^https://(?P<service>github).com/(?P<org>[^/]+)/(?P<repo>[^/]+?)(.git)?$"),
        re.compile(r"^git@(?P<service>gitlab).(?P<instance>[^/]+):(?P<org>.+)/(?P<repo>[^/]+).git$"),
        re.compile(r"^https://(?P<service>gitlab).(?P<instance>[^/]+)/(?P<org>[^/]+)/(?P<repo>[^/]+?)(.git)?$"),
    ]

    for regexp in regexps:
        if match := regexp.match(remote):
            service = match.group('service')
            instance = match.groupdict().get('instance')
            org = match.group('org')
            repo = match.group('repo')
            return filter(None, [service, instance, org, repo])

    return None
