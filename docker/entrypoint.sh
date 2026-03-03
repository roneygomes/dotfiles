#!/bin/sh
set -e

if [ ! -S /var/run/docker.sock ]; then
    echo "error: Docker socket not found. Mount it with -v /var/run/docker.sock:/var/run/docker.sock"
    exit 1
fi

sudo chmod 666 /var/run/docker.sock

exec "$@"
