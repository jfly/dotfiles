#!/usr/bin/env bash

set -e

function provision_azure() {
    PASSWORD=$(pwgen -s 20 1)

    ## The image to use.
    #  'win2016datacenter' works, but requires an os disk of at least 128 GiB, which is expensive.
    # To search for smalldisk images:
    #  az vm image list-skus -l westus -p MicrosoftWindowsServer --offer WindowsServer
    IMAGE=2016-Datacenter-Server-Core-smalldisk


    az vm create \
        --resource-group gaming \
        --name jfly \
        --image $IMAGE \
        --admin-username jfly \
        --admin-password "$PASSWORD" \
        --location westus \
        --size Standard_NV12s_v3 \
        --priority Spot \
        --max-price 0.6 \
        --os-disk-size-gb 35

    echo "Allocated server with password $PASSWORD"
}

function provision_aws() {
    # Urg, blocked on vCPU limits: https://console.aws.amazon.com/support/home?#/case/?caseId=7096997911&displayId=7096997911&language=en

    ### The operating system to use.
    # aws ec2 describe-images --owners self amazon --filters "Name=platform,Values=windows"
    # AMI=ami-0ef87a563bdcc650f  # amazon/Windows_Server-2016-English-Full-Base-2020.06.10
    AMI=ami-026cfeabf26aa8e52  # amazon/Windows_Server-2019-English-Full-Base-2020.06.10

    # Run 'aws ec2 describe-key-pairs' to see a list of valid key pairs.
    # If you do not have one associated with our account yet, you'll need to create one.
    # These directions might help: http://docs.aws.amazon.com/cli/latest/userguide/cli-ec2-keypairs.html#creating-a-key-pair
    # I ran:
    #     aws ec2 create-key-pair --key-name gaming --query 'KeyMaterial' --output text > secret/gaming.pem
    KEY_NAME=gaming
    KEY_PATH=secret/$KEY_NAME.pem

    # NOTE: I modified the default security group to give RDP access: https://us-west-1.console.aws.amazon.com/ec2/v2/home?region=us-west-1#SecurityGroup:groupId=sg-0e8b6fe572f68ec3d
    # TODO: automate this
    # https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/authorizing-access-to-an-instance.html

    json=$(aws ec2 run-instances \
        --image-id $AMI \
        --key-name $KEY_NAME \
        --count 1 \
        --instance-type g4dn.xlarge \
        --block-device-mappings '[ { "DeviceName": "/dev/sda1", "Ebs": { "DeleteOnTermination": true, "VolumeSize": 40, "VolumeType": "standard" } } ]' \
        --instance-market-options '{ "MarketType": "spot", "SpotOptions": { "SpotInstanceType": "persistent", "InstanceInterruptionBehavior": "stop" } }'
    )
    instance_id=$(echo "$json" | jq --raw-output '.Instances[0].InstanceId')
    server_name=jfly
    aws ec2 create-tags --resources "${instance_id}" --tags Key=Name,Value=$server_name
    echo "Allocated new server with instance id: $instance_id and named it ${server_name}"

    echo -n "Waiting for ${server_name} to finish initializing..."
    aws ec2 wait instance-status-ok --instance-ids "${instance_id}"
    echo " done!"

    # Create an elastic ip address and associated it to the server.
    ELASTIC_IP=$(aws ec2 allocate-address | jq --raw-output '.PublicIp')
    echo -n "Assigning ip ${ELASTIC_IP} to server..."
    aws ec2 associate-address --public-ip "${ELASTIC_IP}" --instance-id "${instance_id}"
    echo " done!"

    password=$(aws ec2 get-password-data --instance-id "$instance_id" --priv-launch-key $KEY_PATH | jq --raw-output '.PasswordData')

    echo "New server provisioned! To connect via RDP, run the following command"
    echo "xfreerdp /v:$ELASTIC_IP /u:Administrator '/p:$password'"
}

#provision_azure
provision_aws
