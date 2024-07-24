#!/bin/bash

set -xe

ocamlfind opt -linkpkg -package unix -o main main.ml
