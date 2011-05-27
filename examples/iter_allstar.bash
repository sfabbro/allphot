#!/bin/bash

# get_psf_fwhm <psf file>
get_psf_fwhm() {
    awk 'NR==2 {print ($1+$2)*1.176; exit}' ${1} 2> /dev/null
}

# get_psf_fwhm <psf file>
get_psf_profile() {
    local psfname=$(awk 'NR==1 {print $1; exit}' ${1})
    case ${psfname} in
	GAUSSIAN) echo 1 ;;
	MOFFAT15) echo 2 ;;
	MOFFAT25) echo 3 ;;
	MOFFAT35) echo 4 ;;
	LORENTZ)  echo 5 ;;
	PENNY1)   echo 6 ;;
	PENNY2)   echo 7 ;;
	*) die -q "Bad PSF name: ${psfname}" ;;
    esac
}

make_psf_als() {
    local fits=${1}
    local im=$(basename ${fits%.*})
    export ALLPHOT_PROCDIR=process_${im}
    # 1) make psf with high S/N stars
    allphot daophot opt --dict="${ALLSTAR_DICT}" --out=${im}.opt ${fits}
    allphot daophot find --option TH=15 ${fits}
    allphot daophot phot ${fits}
    allphot daophot pick --nstars=50 --magfaint=13 ${im}.ap
    allphot daophot psf \
	--option VA=-1 \
	--option AN=1 ${fits}
    allphot cat neighbours ${im}.nei ${im}.lst
    allphot daophot psf \
	--option VA=-1 \
	--option AN=1 ${fits}

    # 2) subtract stars neighbours and rebuild
    allphot allstar \
	--option WA=0 \
	--option IS=0 \
	--option OS=0 ${fits}

    allphot daophot sort --index=3 ${im}.als
    allphot daophot substar --in=${im}.srt --keep=${im}.lst ${fits}
    allphot daophot opt --opt=${im}.opt ${im}s.fits
    allphot daophot psf \
	--option VA=-1 \
	--option AN=-7 \
	--pho=${im}.ap \
	--in=${im}.lst \
	--nei=${im}.nei \
	--out=${im}.psf \
	${im}s.fits
    allphot allstar ${fits}

    # 3) detect all faint stars on residuals
    local fwhm=$(get_psf_fwhm ${im}.psf)
    allphot daophot opt \
	--dict="${ALLSTAR_DICT}" \
	--option AN=$(get_psf_profile ${im}.psf) \
	--option FW=${fwhm} \
	--option FI=${fwhm} \
	--option PS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--option VA=2 \
	--option TH=3 \
	--out=${im}.opt ${fits}
    allphot daophot opt --opt=${im}.opt ${im}s.fits 
    allphot daophot find ${im}s.fits
    allphot daophot offset \
	--id=$(tail -n 1 ${im}.coo | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    allphot daophot phot --in=${im}s.off ${im}s.fits
    allphot allstar --psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot daophot sort --renum --index=4 --out=${im}.all ${im}.cmb

    # 4) Make psf fully variable with best profile and subtracted neighbours
    allphot daophot opt --opt=${im}.opt ${im}s.fits
    allphot daophot pick --magfaint=15 --nstars=200 ${im}.all
    for i in $(seq 1 5); do
	allphot daophot substar --in=${im}.nei --keep=${im}.lst ${fits}
	allphot daophot psf --in=${im}.lst --pho=${im}.all --out=${im}.psf --nei=${im}.nei ${im}s.fits
	allphot cat neighbours ${im}.nei ${im}.lst
    done
    allphot allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--in=${im}.all ${fits}
    allphot daophot find ${im}s.fits
    allphot daophot offset \
	--id=$(tail -n 1 ${im}.als | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    allphot daophot phot --in=${im}s.off ${im}s.fits
    allphot allstar --psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot allstar --in=${im}.cmb ${fits}

    allphot daophot find ${im}s.fits
    allphot daophot offset \
	--id=$(tail -n 1 ${im}.coo | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    allphot daophot phot --in=${im}s.off ${im}s.fits
    allphot allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot daophot sort --renum --index=4 --out=${im}.all ${im}.cmb
    allphot daophot pick --magfaint=15 --nstars=200 ${im}.all
    for i in $(seq 1 5); do
	allphot daophot substar --in=${im}.nei --keep=${im}.lst ${fits}
	allphot daophot psf --in=${im}.lst --pho=${im}.all --out=${im}.psf --nei=${im}.nei ${im}s.fits
	allphot cat neighbours ${im}.nei ${im}.lst
    done
    allphot allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--in=${im}.all ${fits}
    allphot daophot find ${im}s.fits
    allphot daophot offset \
	--id=$(tail -n 1 ${im}.als | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    allphot daophot phot --in=${im}s.off ${im}s.fits
    allphot allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot allstar --in=${im}.cmb ${fits}
    unset ALLPHOT_PROCDIR
}

ALLSTAR_DICT=""

while [[ $# -gt 0 ]]; do
    case "${1}" in
	--dict=*) ALLSTAR_DICT="${1##*=}" ;;
	--*) echo "Unrecognized option '$1'"; exit ;;
        *) break ;;
    esac
    shift
done

for i in $@; do
    make_psf_als ${i}
done
