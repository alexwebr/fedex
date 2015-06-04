#!/bin/sh
erl -pa ebin/ -s fedex_app -sname fedex -kernel error_logger '{file, "fedex.log"}' -detached
