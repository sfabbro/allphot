#!/bin/bash

mkdir -p ${DATA_DIR}/_process
cd ${DATA_DIR}/_process

for im in ${DATA_DIR}/sky*.fits; do
    allphot  ${im} ${DATA_DIR}/sky.dict
done

field=sky
ln -s *_process/*.{psf,als,ap,fits} .
ls -1 *.als | tail --lines=+2 > ${field}.list
allphot_daomatch $(ls -1 *.als | head -n1) ${field} ${field}.list
allphot_daomaster ${field}
allphot_allframe ${field}
