#!/bin/sh
export ZOTONIC_DBHOST="localhost"
export ZOTONIC_DBPORT=5432
export ZOTONIC_DBUSER="zophrenic"
export ZOTONIC_DBPASSWORD=""
export ZOTONIC_DB="zotonic"
export ZOTONIC_SIGN_KEY_SIMPLE=gipfelstuermer
cd `dirname $0`
exec erl +P 10000000 +K true -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s zotonic
