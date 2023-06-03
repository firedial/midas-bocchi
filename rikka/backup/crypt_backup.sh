#!/bin/sh

BACKUP_FILE_NAME="backup.dump"
BACKUP_DIR="/mnt/nas/midas/crypt"

/usr/bin/mysqldump --no-tablespaces --single-transaction -u ${DB_TASK_USER} -p${DB_TASK_PASS} -h ${DB_HOST} ${DB_MIDAS_DATABASE_NAME} > /tmp/${BACKUP_FILE_NAME}
gzip /tmp/${BACKUP_FILE_NAME}
BACKUP_GZIP_FILE_NAME=${BACKUP_FILE_NAME}.gz

openssl aes-256-cbc -e -pbkdf2 -iter 100000 -salt -in /tmp/${BACKUP_GZIP_FILE_NAME} -out /tmp/encrypted_${BACKUP_GZIP_FILE_NAME} -pass env:ENCRYPTION_KEY

rm /tmp/${BACKUP_GZIP_FILE_NAME}

mv /tmp/encrypted_${BACKUP_GZIP_FILE_NAME} ${BACKUP_DIR}/encrypted_backup$(date +\%Y\%m\%d\%H\%M\%S).dump
