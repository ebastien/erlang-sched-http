#!/bin/sh
cd `dirname $0`
exec erl -sname sched -setcookie sched -detached -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s wm_sched
