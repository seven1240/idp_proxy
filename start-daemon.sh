#!/bin/sh

cd `dirname $0`
LOG_PATH=erlang_logs/idp_proxy

export RUN_ERL_LOG_GENERATIONS=100
export RUN_ERL_LOG_MAXSIZE=100000000

if [ ! -d $LOG_PATH ]; then
	echo "mkdir -p $LOG_PATH"
	mkdir -p $LOG_PATH
fi
	
echo "Starting..."
run_erl -daemon $LOG_PATH  $LOG_PATH "exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s idp_proxy -sname idp_proxy"

sleep 1
ps aux|grep idp_proxy
