#!/usr/bin/env bash

# Clone all repositories from an organization.

set -e

if [ -z "$GITHUB_AT" ]; then
    echo "You must set the GITHUB_AT environment variable in order for me to query for all repositories."
    echo '1. Go to https://github.com/settings/tokens.'
    echo '2. Click "Generate new token".'
    echo '3. Give it a name (such as "clone all repos"), check the box for "repo", and click "Generate new token".'
    echo '4. Copy the new token to your clipboard, and re-invoke this script:'
    echo "    GITHUB_AT=... $0 $@"
    echo "5. Remember to remove personal access token when you're done with it!"
    exit 1
fi

if [ $# -ne 2 ]; then
    echo "Usage:"
    echo "    GITHUB_AT=... $0 [gh-organization] [directory]"
    echo ""
    echo "If the given directory does not exist, it will be created."
    echo "If the given directory does exist, then missing repositories will be added, and you will get warnings about unexpected repositories."
    echo ""
    echo "For example, to clone all repostories under the 'thewca' GitHub organization into the current directory:"
    echo "    GITHUB_AT=... $0 thewca ."
    exit 1
fi

GH_ORG="$1"
DEST_DIR="$2"
cd $DEST_DIR

expected_repos=$(curl -s "https://$GITHUB_AT:@api.github.com/orgs/$GH_ORG/repos?per_page=200" | jq --raw-output '.[].ssh_url' | sort)
found_repos=$(grep -Pho "(?<=url = ).*"  */.git/config | sort)

missing_repos=$(comm -23 <(echo "$expected_repos") <(echo "$found_repos"))
unexpected_repos=$(comm -13 <(echo "$expected_repos") <(echo "$found_repos"))

# TODO: This is going to warn about a bunch of things it should not warn about.
#       It should get smarter and look at only the remote named upstream.
for repo in $unexpected_repos; do
    echo "WARNING: Found unexpected cloned repository: $repo"
done

for repo in $missing_repos; do
    git clone $repo
done
