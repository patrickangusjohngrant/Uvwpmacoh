#!/bin/bash

. creds.sh

set -e
set -x

if [ `whoami` != "root" ];
then
    exec sudo -E $0
fi

N=1
RELEASE=`date '+%F'`-$N

SOURCE_DIR=`pwd`
IMAGES_DIR=`mktemp -d -p /var/tmp`

ec2-bundle-image \
    -c $SOURCE_DIR/cert.pem \
    -k $SOURCE_DIR/pk.pem \
    -u 694960934408 \
    -i $SOURCE_DIR/fs \
    -r x86_64 \
    --block-device-mapping ami=sda1,root=/dev/sda1 \
    -d $IMAGES_DIR \
    --kernel aki-4feec43b

ec2-upload-bundle \
    -b uvwpmacoh-images2/bundles/uvwpmacoh-$RELEASE \
    -d $IMAGES_DIR \
    --access-key "$AWS_ACCESS_KEY" \
    --secret-key "$AWS_SECRET_KEY" \
    --manifest $IMAGES_DIR/fs.manifest.xml \
    --location eu-west-1 \
    --acl public-read

ec2-register \
    --verbose --debug \
    uvwpmacoh-images2/bundles/uvwpmacoh-$RELEASE/fs.manifest.xml \
    -n uvwpmacoh-$RELEASE \
    -O "$AWS_ACCESS_KEY" \
    -W "$AWS_SECRET_KEY" \
    --virtualization-type paravirtual \
    --region eu-west-1
