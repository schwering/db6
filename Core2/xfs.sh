#!/bin/sh
umount -f /home/chs/Disk
mkfs.xfs -f /dev/sda4 && mount /dev/sda4 /home/chs/Disk && chown chs:chs /home/chs/Disk && build/debug/./ttree && date && build/debug/./heap && date
