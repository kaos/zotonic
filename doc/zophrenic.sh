#!/bin/bash
 
##
## usage zophrenic.sh {start|stop|restart|debug}
##
 
BASE=/home/zophrenic
ME=$BASE/services/zophrenic.sh
PWD=$BASE/zophrenic
ERL="/usr/local/bin/erl"
PA="$PWD/ebin $PWD/deps/*/ebin"
HOSTNAME=`hostname`
SNAME=zophrenic001

export HOME=$BASE
export HEART_COMMAND="$ME start"
export ZOPHRENIC_SIGN_KEY_SIMPLE="applepie"

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

