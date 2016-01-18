#!/bin/bash

export EC2_AMITOOL_HOME=/usr/local/ec2/ec2-ami-tools-1.5.7
export PATH=$EC2_AMITOOL_HOME/bin:/usr/sbin:$PATH

. creds.sh

set -e
set -x

if [ `whoami` != "root" ];
then
    exec sudo -E $0
fi

RELEASE=`date '+%F'`

BUILD_DIR=`mktemp -d -p /var/tmp`
SOURCE_DIR=`pwd`
IMAGES_DIR=`mktemp -d -p /var/tmp`

cd $BUILD_DIR
gzip -dc $SOURCE_DIR/uvwpmacoh.cpio.gz | cpio -iv
#cp $SOURCE_DIR/vmlinuz $BUILD_DIR/

SIZE=100 # `du -m $BUILD_DIR | awk '{print int($1 * 1.5)}'`

mkdir -p $BUILD_DIR/{boot/grub,usr/sbin}
cp -av $SOURCE_DIR/grub.conf $BUILD_DIR/boot/grub/
cp -av /usr/sbin/grub $BUILD_DIR/usr/sbin/

ec2-bundle-vol \
    -c $SOURCE_DIR/cert.pem \
    -k $SOURCE_DIR/pk.pem \
    -u 694960934408 \
    -r x86_64 \
    --block-device-mapping ami=sda1,root=/dev/sda1 \
    -s $SIZE \
    --no-inherit \
    -v $BUILD_DIR \
    -d $IMAGES_DIR \
    --kernel aki-4feec43b

ec2-upload-bundle \
    -b uvwpmacoh-images2/bundles/uvwpmacoh-$RELEASE \
    -d $IMAGES_DIR \
    --access-key "$AWS_ACCESS_KEY" \
    --secret-key "$AWS_SECRET_KEY" \
    --manifest $IMAGES_DIR/image.manifest.xml \
    --location eu-west-1 \
    --acl public-read

ec2-register \
    --verbose --debug \
    uvwpmacoh-images2/bundles/uvwpmacoh-$RELEASE/image.manifest.xml \
    -n uvwpmacoh-$RELEASE \
    -O "$AWS_ACCESS_KEY" \
    -W "$AWS_SECRET_KEY" \
    --virtualization-type hvm \
    --region eu-west-1
