#!/bin/bash
 
##
## usage zotonic.sh {start|stop|restart|debug}
##
## Use "debug" to start an interactive shell (highly recommended when installing the db on the first run).
## Use "start" in production. This boots zotonic in an erlang vm with a heart beat process.
 
BASE=/home/zotonic
ME=$BASE/zotonic.sh
PWD=$BASE/zotonic
ERL="/usr/local/bin/erl"
PA="$PWD/ebin $PWD/deps/*/ebin"

# The name of the Erlang node.
HOSTNAME=`hostname`
SNAME=zotonic001

export HOME=$BASE
export HEART_COMMAND="$ME start"

## The port and IP address zotonic will bind to (defaults to all ip addresses and port 8000)
# export ZOTONIC_IP=127.0.0.1
# export ZOTONIC_PORT=8000

## The admin password
export ZOTONIC_ADMINPASSWORD="admin"

## Key used for signing image urls with image manipulations (crop, rotate, resize, etc.)
## When not set then zotonic will generate a new key on every restart, forcing old image urls to fail.
## A new key will also force regenerating images, which takes cpu time and will fill your hard disk.
export ZOTONIC_SIGN_KEY_SIMPLE="--change-me--"

## Key used for signing postbacks - this _must_ be a hard to guess key, otherwise your system is insecure.
## When not defined, then zotonic will generate a new key on every restart.
## When a new key is generated then all postbacks from old html pages will fail.
# export ZOTONIC_SIGN_KEY="--change-me-as-well--"

## Database settings, the settings below are the defaults.
# export ZOTONIC_DBHOST="localhost"
# export ZOTONIC_DBPORT=5432
# export ZOTONIC_DBUSER="zotonic"
# export ZOTONIC_DBPASSWORD=""
# export ZOTONIC_DB="zotonic"

pushd $PWD >/dev/null


function start() {
    echo  "Starting zotonic"
    make >/dev/null
    $ERL -pa $PA -sname $SNAME -boot start_sasl -heart -detached -s zotonic
}

function stop() {
    echo "Stopping zotonic"
    $ERL -noshell -pa $PA -sname ${SNAME}_stop -s zotonic stop $SNAME@$HOSTNAME
}


case $1 in

  start)
    start
    ;;
 
  debug)
    $ERL -pa $PA -sname $SNAME -boot start_sasl -s zotonic
    ;;
 
  stop)
    stop
    ;;
 
  restart)
    echo "Restarting zotonic"
    stop
    start
    ;;

  *)
    echo "Usage: $0 {start|stop|restart|debug}"
    exit 1
esac

popd > /dev/null
 
exit 0

