#!/bin/bash
 
##
## usage zophrenic.sh {start|stop|restart|debug}
##
## Use "debug" to start an interactive shell (highly recommended when installing the db on the first run).
## Use "start" in production. This boots Zophrenic in an erlang vm with a heart beat process.
 
BASE=/home/zophrenic
ME=$BASE/services/zophrenic.sh
PWD=$BASE/zophrenic
ERL="/usr/local/bin/erl"
PA="$PWD/ebin $PWD/deps/*/ebin"

# The name of the Erlang node.
HOSTNAME=`hostname`
SNAME=zophrenic001

export HOME=$BASE
export HEART_COMMAND="$ME start"

## The port and IP address zophrenic will bind to (defaults to all ip addresses and port 8000)
# export ZP_IP=127.0.0.1
# export ZP_PORT=8000

## The admin password
export ZP_ADMINPASSWORD="admin"

## Key used for signing image urls with image manipulations (crop, rotate, resize, etc.)
## When not set then Zophrenic will generate a new key on every restart, forcing old image urls to fail.
## A new key will also force regenerating images, which takes cpu time and will fill your hard disk.
export ZOPHRENIC_SIGN_KEY_SIMPLE="--change-me--"

## Key used for signing postbacks - this _must_ be a hard to guess key, otherwise your system is insecure.
## When not defined, then Zophrenic will generate a new key on every restart.
## When a new key is generated then all postbacks from old html pages will fail.
# export ZOPHRENIC_SIGN_KEY="--change-me-as-well--"

## Database settings, the settings below are the defaults.
# export ZP_DBHOST="localhost"
# export ZP_DBPORT=5432
# export ZP_DBUSER="zophrenic"
# export ZP_DBPASSWORD=""
# export ZP_DB="zophrenic"

pushd $PWD >/dev/null


function start() {
    echo  "Starting zophrenic"
    make >/dev/null
    $ERL -pa $PA -sname $SNAME -boot start_sasl -heart -detached -s zophrenic
}

function stop() {
    echo "Stopping zophrenic"
    $ERL -noshell -pa $PA -sname ${SNAME}_stop -s zophrenic stop $SNAME@$HOSTNAME
}


case $1 in

  start)
    start
    ;;
 
  debug)
    $ERL -pa $PA -sname $SNAME -boot start_sasl -s zophrenic
    ;;
 
  stop)
    stop
    ;;
 
  restart)
    echo "Restarting zophrenic"
    stop
    start
    ;;

  *)
    echo "Usage: $0 {start|stop|restart|debug}"
    exit 1
esac

popd > /dev/null
 
exit 0

