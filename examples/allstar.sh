#!/bin/sh

# psf analytical profile 
prof=6
# name of the dictionary 
field=${1}

if [ $# -lt 2 ]; then
    echo "Usage: $(basename $0) <dict> <fits image>"
fi

im=${2%.*}
allphot daophot opt --dict=${field}.dict ${im}.fits
allphot daophot psf ${im}.fits
allphot filters neighbours daophot_${im%.*}/${im}.{nei,lst}
allphot daophot psf --prof=${prof} ${im}.fits
for i in $(seq 1 5); do
    allphot filters neighbours daophot_${im%.*}/${im}.{nei,lst}
    allphot daophot psf --var=2 ${im}.fits
done
allphot daophot allstar ${im}.fits
