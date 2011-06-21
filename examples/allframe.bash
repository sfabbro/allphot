#!/bin/bash

if [ $# -lt 1 ]; then
    echo "Usage: $(basename $0) <file1.als>...<file m.als>"
    echo " First one is reference"
fi

mch=${1%.*}.mch
allphot daomatch --out=${mch} $*
allphot daomaster ${mch}
allphot allframe ${mch}
allphot montage2 ${mch}

