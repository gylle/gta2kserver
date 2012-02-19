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
  --shell)
    set -x
    erl $ERL_FLAGS -s gta2k start_server
    set +x
    return=$?
    ;;
  *)
    erl $ERL_FLAGS -detached -s gta2k start_server
    return=$?
esac
