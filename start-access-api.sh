#!/bin/sh

exec /usr/local/bin/erl -noshell -pa /root/mgapistat/ebin -run access_api main -run init stop
