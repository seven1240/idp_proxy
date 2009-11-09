#!/bin/sh
cd `dirname $0`
erl_call -a 'idp_proxy stop' -sname 'idp_proxy'

echo
ps aux|grep idp_proxy
