#!/bin/bash

source options.bash
source recipes.bash

DATA_DIR=$(readlink -f ../data)

mkdir -p ${DATA_DIR}/tests
cd ${DATA_DIR}/tests

for im in ${DATA_DIR}/sky*.fits; do
    allphot_allstar_iterate ${im} ${DATA_DIR}/sky.assoc
done

field=sky
ln -s *_process/*.{psf,als,ap,fits} .
ls -1 *.als | tail --lines=+2 > ${field}.list
allphot_daomatch $(ls -1 *.als | head -n1) ${field} ${field}.list
allphot_daomaster ${field}
allphot_allframe ${field}
