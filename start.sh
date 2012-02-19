#!/bin/bash -x

DIR=$(readlink -f $(dirname $0))
ERL_FLAGS="-smp -pa $DIR/ebin"

case "$1" in
  --debug)
    set -x
    erl $ERL_FLAGS
    set +x
    return=$?
    ;;
  *)
    set -x
    erl $ERL_FLAGS -s gta2k start_server
    set +x
    return=$?
esac
