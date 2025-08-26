#!/bin/bash

set -xe

cc -ggdb -O0 -std=c23 -o jt $(find . -type f -name '*.c') $(forge lib)
