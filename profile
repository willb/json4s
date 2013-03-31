#!/bin/bash

function die() {
    echo "$*" 1>&2
    exit 1
}

MAINCLASS=org.json4s.benchmark.Runner

exec benchmark/target/start $MAINCLASS