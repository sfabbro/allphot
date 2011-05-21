#!/bin/bash

if [ $# -lt 1 ]; then
    echo "Usage: $(basename $0) <file1.als>...<file m.als>"
    echo " First one is reference"
fi

allphot daomatch do --out=${1%.*}.mch $*
allphot daomaster do sky.mch
allphot allframe do sky.mch

