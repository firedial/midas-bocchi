#!/bin/sh

DATA="{\"content\":\"$1\"}"
curl -X POST -H 'Content-type: application/json' --data "${DATA}" ${DISCORD_URL}
