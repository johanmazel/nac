#!/bin/sh

if [ -f "setup.data" ]; then
	rm setup.data
fi

if [ -f "setup.log" ]; then
	rm setup.log
fi

if [ -f "setup.ml" ]; then
	rm setup.ml
fi
