#!/usr/bin/env bash

set -e

profile=honor-temp

if aws --profile="$profile" iam get-user > /dev/null; then
    # The temp credentials are still working! Nothing to do here.
    exit 0
fi

# Note we're careful to only print to /dev/stderr. That's because this script
# might be called via a clever ssh ProxyCommand, and printing to stdout would
# actually inject bytes into the SSH TCP connection!
echo "" > /dev/stderr
echo "Your credentials for AWS profile $profile appear to be out of date. Refreshing..." > /dev/stderr
echo "Please enter a MFA code in the dialog that appears." > /dev/stderr
# We can't read from stdin here because it would mess up the ssh protocol (see
# comment above about /dev/stderr).
MFA=$(zenity --title="Enter MFA code" --text="Enter MFA code" --entry)

if [ -z "$MFA" ]; then
    echo "You must enter a MFA" > /dev/stderr
    exit 1
fi

TOKEN=$(aws --profile=honor sts get-session-token --serial-number arn:aws:iam::900965112463:mfa/jeremy --token-code "$MFA")
ACCESS_KEY=$(echo "$TOKEN" | jq -r '.Credentials.AccessKeyId')
SECRET_KEY=$(echo "$TOKEN" | jq -r '.Credentials.SecretAccessKey')
SESSION=$(echo "$TOKEN" | jq -r '.Credentials.SessionToken')

echo "=> updating ~/.aws/credentials as profile $profile" > /dev/stderr
aws configure set --profile "$profile" aws_access_key_id "$ACCESS_KEY"
aws configure set --profile "$profile" aws_secret_access_key "$SECRET_KEY"
aws configure set --profile "$profile" aws_session_token "$SESSION"
echo "[OK] done" > /dev/stderr
