#!/bin/bash

do_psf_allstar() {
    local options
    local gain=$(fitskey -n -p GAIN ${1})
    [[ ${gain} == absent ]] || options="${options} --option GA=${gain}"
    local ron=$(fitskey -n -p RDNOISE ${1})
    [[ ${ron} == absent ]] || options="${options} --option RE=${ron}"
    allphot daophot opt ${options}
    allphot daophot find ${1}
    allphot daophot phot ${1}
    allphot daophot pick ${1%.*}.ap
    local i
    for i in $(seq 3); do
	allphot cat neighbours ${1%.*}.{nei,lst}
	allphot daophot psf ${1}
    done
    allphot allstar ${1}
}

if [ $# -lt 2 ]; then
    echo "Usage: $(basename $0) <fits image 1>...<fits image n>"
fi

for im in *@; do
    do_psf_allstar ${im}
done
