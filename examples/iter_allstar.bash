#!/bin/bash

# get_psf_fwhm <psf file>
# returns the FWHM from the psf file
get_psf_fwhm() {
    awk 'NR==2 {print ($1+$2)*1.176; exit}' ${1} 2> /dev/null
}

# get_psf_profile <psf file>
# returns the analytical profile type
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

# iter_psf_allstar <fits file>
# iterate between PSF and ALLSTAR to create a good PSF and star list
iter_psf_allstar() {
    local fits=${1}
    local im=$(basename ${fits%.*})
    export ALLPHOT_PROCDIR=process_${im}
    
    echo " ========================================================"
    echo " === STEP 1: Make simple Gaussian PSF with high S/N stars"
    echo " ========================================================"

    allphot daophot opt --dict="${DICTFILE}" --out=${im}.opt ${fits}
    allphot daophot find --option TH=15 ${fits}
    allphot daophot phot ${fits}
    allphot daophot pick --nstars=50 --magfaint=13 ${im}.ap
    allphot daophot psf \
	--option VA=-1 \
	--option AN=1 ${fits}
    allphot cat neighbours ${im}.nei ${im}.lst    
    allphot daophot psf \
	--option VA=-1 \
	--option AN=1 \
	${fits}

    allphot allstar \
	--option WA=0 \
	--option IS=0 \
	--option OS=0 ${fits}

    echo " ================================================================"
    echo " === STEP 2: Remove neighbours and choose best analytical profile"
    echo " ================================================================"

    allphot daophot sort --index=3 ${im}.als
    allphot daophot substar --in=${im}.srt --keep=${im}.lst ${fits}
    allphot daophot opt --opt=${im}.opt ${im}s.fits
    allphot daophot psf \
	--option VA=-1 \
	--option AN=-7 \
	--pho=${im}.ap \
	--in=${im}.lst \
	--out=${im}.psf \
	${im}s.fits
    allphot allstar ${fits}
    rm -f ${im}.srt ${ALLPHOT_PROCDIR}/${im}.srt

    echo " ================================================================"
    echo " === STEP 3: Increase spatial variabilty and detect fainter stars"
    echo " ================================================================"

    local fwhm=$(get_psf_fwhm ${im}.psf)
    local radii="--radius A1=$(echo ${fwhm} | awk '{print $1*1.2}')"
    radii="${radii} --radius IS=$(echo ${fwhm} | awk '{print $1*3}')"
    radii="${radii} --radius OS=$(echo ${fwhm} | awk '{print $1*6}')"

    allphot daophot opt \
	--dict="${DICTFILE}" \
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
    allphot daophot phot ${radii} --in=${im}s.off ${im}s.fits
    allphot allstar --psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot daophot sort --renum --index=4 --out=${im}.all ${im}.cmb

    echo " ================================================================"
    echo " === STEP 4: Iterate between ALLSTAR and PSF to get unpolluted PSF"
    echo " ================================================================"

    allphot daophot opt --opt=${im}.opt ${im}s.fits
    allphot daophot phot ${radii} --in=${im}.all --out=${im}.ap ${im}.fits
    allphot daophot pick --magfaint=15 --nstars=200 ${im}.ap
    for i in $(seq 1 5); do
	allphot daophot substar --in=${im}.nei --keep=${im}.lst ${fits}
	allphot daophot psf \
	    --in=${im}.lst \
	    --pho=${im}.all \
	    --out=${im}.psf \
	    ${im}s.fits
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
    allphot daophot phot ${radii} --in=${im}s.off ${im}s.fits
    allphot allstar --psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot allstar --in=${im}.cmb ${fits}

    allphot daophot find ${im}s.fits
    allphot daophot offset \
	--id=$(tail -n 1 ${im}.coo | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    allphot daophot phot ${radii} --in=${im}s.off ${im}s.fits
    allphot allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot daophot sort --renum --index=4 --out=${im}.all ${im}.cmb
    allphot daophot phot ${radii} --in=${im}.all --out=${im}.ap ${im}.fits
    allphot daophot pick --magfaint=15 --nstars=200 ${im}.ap
    for i in $(seq 1 5); do
	allphot daophot substar --in=${im}.nei --keep=${im}.lst ${fits}
	allphot daophot psf \
	    --in=${im}.lst \
	    --pho=${im}.all \
	    --out=${im}.psf \
	    ${im}s.fits
	allphot cat neighbours ${im}.nei ${im}.lst
    done
    rm -f ${im}.nei

    echo " ================================================================"
    echo " === STEP 5: Final ALLSTARs with stable PSF with every star      "
    echo " ================================================================"

    allphot allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--in=${im}.all ${fits}
    rm -f ${im}.all

    allphot daophot find ${im}s.fits
    allphot daophot offset \
	--id=$(tail -n 1 ${im}.als | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    allphot daophot phot ${radii} --in=${im}s.off ${im}s.fits
    allphot allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot allstar --in=${im}.cmb ${fits}
    cp -f ${ALLPHOT_PROCDIR}/*.opt .
    rm -f ${im}.cmb ${im}s.{off,als,ap,coo} ${im}ss.fits
    rm -rf ${ALLPHOT_PROCDIR} 
    unset ALLPHOT_PROCDIR
}

usage() {
    echo "$(basename ${0}) [--dict=<dictfile>] <FITS file>...<FITS file>"
    exit
}

DICTFILE=""

[[ $# -lt 1 ]] && usage

while [[ $# -gt 0 ]]; do
    case "${1}" in
	--dict=*) DICTFILE="${1##*=}" ;;
	--help) usage ;;
	--*) echo "Unrecognized option: ${1}" 1>&2 ; exit ;;
        *) break ;;
    esac
    shift
done

for f in $@; do
    iter_psf_allstar ${f}
done
