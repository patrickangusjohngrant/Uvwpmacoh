#!/bin/bash

set -e
set -x

if [ `whoami` != "root" ];
then
    exec sudo -E $0
fi

whoami

BUILD_DIR=`mktemp -d`
SOURCE_DIR=`pwd`
IMAGES_DIR=`mktemp -d`

cp $SOURCE_DIR/uvwpmacoh.cpio.gz $BUILD_DIR/initrd.img
cp $SOURCE_DIR/vmlinuz $BUILD_DIR/

SIZE=100 # `du -m $BUILD_DIR | awk '{print int($1 * 1.5)}'`

mkdir -p $BUILD_DIR/boot/grub
cp -v $SOURCE_DIR/grub.conf $BUILD_DIR/boot/grub/

ec2-bundle-vol -c /home/patrick/Downloads/cert-SFS43XWDIIIQDJVDZRFXMMCP3VG2D65E.pem -k /home/patrick/Downloads/pk-SFS43XWDIIIQDJVDZRFXMMCP3VG2D65E.pem -u 694960934408 -r x86_64 --block-device-mapping ami=sda1,root=/dev/sda1 -s $SIZE --no-inherit -v $BUILD_DIR -d $IMAGES_DIR --kernel aki-4feec43b
ec2-upload-bundle -b uvwpmacoh-images2/bundles/uvwpmacoh-2015.01.15-1 -d $IMAGES_DIR --access-key "$AWS_ACCESS_KEY" --secret-key "$AWS_SECRET_KEY" --manifest $IMAGES_DIR/image.manifest.xml  --location eu-west-1  --acl public-read
ec2-register --verbose --debug uvwpmacoh-images2/bundles/uvwpmacoh-2015.01.15-1/image.manifest.xml -n uvwpmacoh-2015.01.15-1 -O "$AWS_ACCESS_KEY" -W "$AWS_SECRET_KEY" --virtualization-type paravirtual --region eu-west-1
