#!/bin/bash

set -e
set -x

BUILD_DIR=`mktemp -d`
SOURCE_DIR=`pwd`


DEST_FILESYSTEM=/dev/mapper/`kpartx -av fs | awk '{print $3}'`
sleep 0.5

mkfs.ext3 -F $DEST_FILESYSTEM

mount -o loop $DEST_FILESYSTEM $BUILD_DIR

mkdir $BUILD_DIR/busybox
cp /bin/busybox $BUILD_DIR/busybox/
$BUILD_DIR/busybox/busybox --install $BUILD_DIR/busybox/

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
        cp -rav $1 $DEST
        if [ -h $1 ];
        then
            pushd `dirname $1`
            bundle `readlink $1` $2/`dirname $1`
            popd
        fi
    fi
}

bundle_libs() {
    for i in `ldd $1 | egrep -o ' (/.*) '`;
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
bundle /bin/bash
bundle_libs /bin/bash
bundle /bin/sh
bundle /bin/dash
bundle /bin/mount
bundle_libs /bin/mount
bundle /lib64/ld-linux-x86-64.so.2
bundle /sbin/fsck
bundle /sbin/fsck.ext3
bundle /sbin/fsck.ext4
bundle /lib/systemd/systemd-udevd
bundle_libs /lib/systemd/systemd-udevd
bundle /bin/udevadm
bundle /sbin/udevadm
bundle_libs /bin/udevadm

mkdir -p $BUILD_DIR/usr/bin
mkdir -p $BUILD_DIR/sys
mkdir -p $BUILD_DIR/proc
mkdir -p $BUILD_DIR/dev
mkdir -p $BUILD_DIR/dev/pts
mkdir -p $BUILD_DIR/tmp
mkdir -p $BUILD_DIR/root
mkdir -p $BUILD_DIR/fuse
chmod 1777 $BUILD_DIR/tmp
mkdir -p $BUILD_DIR/run

cat << EOF > $BUILD_DIR/etc/fstab
/dev/sda1 / ext3 defaults 0 0
EOF

umount $BUILD_DIR
sleep 1

kpartx -dv $SOURCE_DIR/fs

#gzip -dc /boot/initrd.img-`uname -r` > $SOURCE_DIR/uvwpmacoh.cpio
##zcat /boot/initrd.img-`uname -r` | cpio -i
#
#mkdir -p  lib/udev
#cp  /lib/udev/hwdb.bin lib/udev
#find | cpio -H newc -A -o -F $SOURCE_DIR/uvwpmacoh.cpio
#
#gzip -f $SOURCE_DIR/uvwpmacoh.cpio
#
##rm -fr $BUILD_DIR
#cd $SOURCE_DIR
#
## Copy my kernel here (rather than just using it out of /boot as this syncs it
## with github... which makes it easier for sharing)
#
#cp /boot/vmlinuz-`uname -r` ./vmlinuz
