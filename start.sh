#!/bin/sh
export ZOPHRENIC_SIGN_KEY_SIMPLE=gipfelstuermer
cd `dirname $0`
exec erl +P 10000000 +K true -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s zophrenic
