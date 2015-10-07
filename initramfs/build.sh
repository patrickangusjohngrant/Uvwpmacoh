#!/bin/bash

set -e

BUILD_DIR=`mktemp -d`
SOURCE_DIR=`pwd`

cp /bin/busybox $BUILD_DIR
cp /etc/udhcpc/default.script $BUILD_DIR/dhcp_client
cp ../filesystem/init $BUILD_DIR/init

# Probably overengineered. Likely broken. Works though.
bundle() {
    if [ -d $1 ]
    then
        mkdir $BUILD_DIR/$2/$1
    else
        DEST=$BUILD_DIR/$2/`dirname $1`
        mkdir -p $DEST
        cp -rv $1 $DEST
        if [ -h $1 ];
        then
            pushd `dirname $1`
            bundle `readlink $1` $2/`dirname $1`
            popd
        fi
    fi
}

bundle_libs() {
    for i in `ldd ../filesystem/init | egrep -o ' (/.*) '`;
    do
        bundle $i
    done
}

bundle `which strace`
bundle_libs `which strace`
bundle_libs ../filesystem/init
bundle /etc/nsswitch.conf
bundle /lib/x86_64-linux-gnu/libnss_dns.so.2
bundle /lib/x86_64-linux-gnu/libc.so.6
bundle /lib/x86_64-linux-gnu/libresolv.so.2
bundle /etc/localtime
bundle /usr/share/zoneinfo/UTC

cp /usr/lib/PXELINUX/pxelinux.0 $BUILD_DIR

mkdir -p $BUILD_DIR/usr/bin

cd $BUILD_DIR

gzip -dc /boot/initrd.img-`uname -r` > $SOURCE_DIR/uvwpmacoh.cpio
#zcat /boot/initrd.img-`uname -r` | cpio -i

mkdir -p  lib/udev
cp  /lib/udev/hwdb.bin lib/udev
find | cpio -H newc -A -o -F $SOURCE_DIR/uvwpmacoh.cpio

gzip -f $SOURCE_DIR/uvwpmacoh.cpio

#rm -fr $BUILD_DIR
cd $SOURCE_DIR

# Copy my kernel here (rather than just using it out of /boot as this syncs it
# with github... which makes it easier for sharing)

cp /boot/vmlinuz-`uname -r` ./vmlinuz
