#!/bin/bash

# psf analytical profile 
prof=1

# name of the dictionary 
dict=sky.dict

ALLPHOT_EXE=../bin/allphot
export ALLPHOT_DATA_PATH=".." ALLPHOT_LIBEXEC_PATH=".." 

for i in sky00.fits; do
    im=${i%.*}
    ${ALLPHOT_EXE} daophot opt --dict=${dict} ${im}.fits
    ${ALLPHOT_EXE} daophot find ${im}.fits
    ${ALLPHOT_EXE} daophot phot ${im}.fits
    ${ALLPHOT_EXE} daophot pick ${im}.ap
    ${ALLPHOT_EXE} daophot psf ${im}.fits
    ${ALLPHOT_EXE} cat neighbours ${im}.{nei,lst}
    ${ALLPHOT_EXE} daophot psf --option AN=-${prof} ${im}.fits
    ${ALLPHOT_EXE} cat neighbours ${im}.{nei,lst}
    ${ALLPHOT_EXE} daophot psf --option AN=-${prof} ${im}.fits
    #${ALLPHOT_EXE} daophot peak ${im}.fits
    #${ALLPHOT_EXE} daophot allstar ${im}.fits
done
