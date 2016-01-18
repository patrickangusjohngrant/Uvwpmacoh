#!/bin/bash

#qemu-system-x86_64 -nographic -kernel ./vmlinuz -initrd uvwpmacoh.cpio.gz /dev/zero -m 1024 -redir tcp:8000::8000 -redir tcp:2222::22 -append console=ttyS0

#qemu-system-x86_64 -nographic -kernel /boot/vmlinuz-`uname -r` -initrd /boot/initrd.img-`uname -r` fs -m 1024 -redir tcp:8000::8000 -redir tcp:2222::22 -append 'console=ttyS0 root=/dev/sda1 init=/init'
          qemu-system-x86_64 -kernel /boot/vmlinuz-`uname -r` -initrd /boot/initrd.img-`uname -r` fs -m 1024 -redir tcp:8000::8000 -redir tcp:2222::22 -append 'root=/dev/sda1 init=/init'
