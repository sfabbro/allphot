#!/bin/bash

export ALLPHOT_OPT_DIR=$(readlink -f ../data)
export ALLPHOT_EXEC_DIR=$(readlink -f ../scripts)
export PATH="$(readlink -f ../src):${PATH}"
source ${ALLPHOT_EXEC_DIR}/recipes.bash

DATA_DIR=$(readlink -f ../tests)
mkdir -p ${DATA_DIR}/_process
cd ${DATA_DIR}/_process

for im in ${DATA_DIR}/sky*.fits; do
    allphot_allstar_iterate ${im} ${DATA_DIR}/sky.dict
done

field=sky
ln -s *_process/*.{psf,als,ap,fits} .
ls -1 *.als | tail --lines=+2 > ${field}.list
allphot_daomatch $(ls -1 *.als | head -n1) ${field} ${field}.list
allphot_daomaster ${field}
allphot_allframe ${field}
