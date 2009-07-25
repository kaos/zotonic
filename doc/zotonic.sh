#!/bin/bash
 
##
## usage zotonic.sh {debug|start|stop|restart}
##
## Use "debug" to start an interactive shell (highly recommended when installing the db on the first run).
## Use "start" in production. This boots zotonic in an erlang vm with a heart beat process.
##
## The proposed directory structure is:
## /home/zotonic/zotonic.sh                         -- this script
## /home/zotonic/zotonic/...                        -- the zotonic code
## /home/zotonic/zotonic/priv/sites/default/...     -- your site and uploaded files go here

# Change this to your base directory
BASE=/home/zotonic

# Change this to the complete path to zotonic.sh (this script)
ZOTONIC_SH=$BASE/zotonic.sh

# Change this to the directory where you have unpacked zotonic
# IMPORTANT: this directory must be called zotonic or zotonic-x.y where x.y is the version number.
ZOTONIC=$BASE/zotonic

# Change this to point to the erlang vm
ERL="/usr/local/bin/erl"

# The include path for the erlang vm, add when needed for your application.
PA="$ZOTONIC/ebin $ZOTONIC/deps/*/ebin"

# The name of the Erlang node, change to 'localhost' when you have problems with your hostname.
HOSTNAME=`hostname`
SNAME=zotonic001

# The command used to restart zotonic when crashed, only used after a "zotonic.sh start"
export HEART_COMMAND="$ZOTONIC_SH start"

## The port and IP address zotonic will bind to (defaults to all ip addresses and port 8000)
export ZOTONIC_IP=any
export ZOTONIC_PORT=8000

## The admin password - this is the initial password, you can change it in the /admin
export ZOTONIC_ADMINPASSWORD="admin"

## Key used for signing image urls with image manipulations (crop, rotate, resize, etc.)
## When not set then zotonic will generate a new key on every restart, forcing old image urls to fail.
## A new key will also force regenerating images, which takes cpu time and will fill your hard disk.
export ZOTONIC_SIGN_KEY_SIMPLE="--change-me--"

## Key used for signing postbacks - this _must_ be a hard to guess key, otherwise your system is insecure.
## When not defined, then zotonic will generate a new key on every restart.
## When a new key is generated then all postbacks from old html pages will fail.
export ZOTONIC_SIGN_KEY="--change-me-as-well--"

## Database settings, the settings below are the defaults.
export ZOTONIC_DBHOST="localhost"
export ZOTONIC_DBPORT=5432
export ZOTONIC_DBUSER="zotonic"
export ZOTONIC_DBPASSWORD=""
export ZOTONIC_DB="zotonic"

pushd $PWD >/dev/null


function start() {
    echo  "Starting zotonic $SNAME"
    make >/dev/null
    $ERL -pa $PA -sname $SNAME -boot start_sasl -heart -detached -s zotonic
}

function stop() {
    echo "Stopping zotonic $SNAME"
    $ERL -noshell -pa $PA -sname ${SNAME}_stop -s zotonic stop $SNAME@$HOSTNAME
}

function update() {
    echo "Updating zotonic $SNAME"
    $ERL -noshell -pa $PA -sname ${SNAME}_stop -s zotonic update $SNAME@$HOSTNAME
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
 
  update)
    update
    ;;

  restart)
    echo "Restarting zotonic"
    stop
    start
    ;;

  *)
    echo "Usage: $0 {debug|start|stop|restart|update}"
    exit 1
esac

popd > /dev/null
 
exit 0

