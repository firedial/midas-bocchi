#!/bin/sh

mount -t cifs ${NAS_PATH} /mnt/nas -o username=${NAS_USER},password=${NAS_PASS},iocharset=utf8,rw

/usr/sbin/crond
crontab /app/init/crontab_setting
