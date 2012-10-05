#!/bin/bash

# psf_fwhm PSF
# returns the FWHM from the PSF file
psf_fwhm() {
    awk 'NR==2 {print ($1+$2)*1.176; exit}' ${1} 2> /dev/null
}

# psf_profile PSF
# returns the analytical profile number from the PSF file
psf_profile() {
    local prof=$(awk 'NR==1 {print $1; exit}' ${1})
    case ${prof} in
	GAUSSIAN) echo 1 ;;
	MOFFAT15) echo 2 ;;
	MOFFAT25) echo 3 ;;
	MOFFAT35) echo 4 ;;
	LORENTZ)  echo 5 ;;
	PENNY1)   echo 6 ;;
	PENNY2)   echo 7 ;;
	*) die -q "Unknown PSF profile: ${prof}" ;;
    esac
}

psf_remove_neighbours() {
    local fits=${1}
    local im=$(basename ${fits%.*})
    local daoopt=${im}_dao.opt

    set -e
    # need daophot.opt, ${im}.psf
    until [[ nbad -eq 0 ]]; do
	${ALLPHOT} daophot psf --in=${im}.lst --pho=${im}.als --out=${im}.psf \
	    ${im}s.fits > ${im}.psf.log
	${ALLPHOT} cat badpsf ${im}.psf.log ${im}.lst
	nbad=$(grep *? saturated defective bad star)
	${ALLPHOT} daophot substar --in=${im}.nei --keep=${im}.lst ${fits}
    done
    rm bad.log
    set +e
}

psf_make_analytic() {
    local fits=${1}
    local im=$(basename ${fits%.*})
    local daoopt=${im}_dao.opt
    local phoopt=${im}_pho.opt
    local alsopt=${im}_als.opt

    set -e

    ${ALLPHOT} daophot opt --dict="${DICTFILE}" --out=${daoopt} ${fits}
    ${ALLPHOT} daophot find --option TH=15 ${fits}
    ${ALLPHOT} daophot phot ${fits} > pho.log
    local maxmag="$(awk '/Estimated/ {print $6-$8*4}' pho.log)"
    rm pho.log
    ${ALLPHOT} daophot pick \
	--nstars=50 \
	--magfaint=${maxmag} ${im}.ap
    ${ALLPHOT} daophot psf \
	--option VA=-1 \
	--option AN=-3 ${fits}
    ${ALLPHOT} cat neighbours ${im}.nei ${im}.lst
    ${ALLPHOT} daophot psf \
	--option VA=-1 \
	--option AN=$(get_psf_profile ${im}.psf) \
	${fits}

    ${ALLPHOT} allstar \
	--option WA=0 \
	--option IS=0 \
	--option OS=0 ${fits}

    ${ALLPHOT} daophot sort --index=3 ${im}.als
    ${ALLPHOT} daophot substar --in=${im}.srt --keep=${im}.lst ${fits}
    ${ALLPHOT} daophot opt --opt=${im}.opt ${im}s.fits
    ${ALLPHOT} daophot psf \
	--option VA=-1 \
	--option AN=-7 \
	--pho=${im}.ap \
	--in=${im}.lst \
	--out=${im}.psf \
	${im}s.fits
    rm -f ${im}.srt

    set +e
}

psf_make_variable() {
    local fits=${1}
    local im=$(basename ${fits%.*})
    local daoopt=${im}_dao.opt
    local phoopt=${im}_pho.opt
    local alsopt=${im}_als.opt
    
    set -e

    local fwhm=$(psf_fwhm ${im}.psf)
    ${ALLPHOT} daophot opt \
	--dict="${DICTFILE}" \
	--option AN=$(psf_profile ${im}.psf) \
	--option FW=${fwhm} \
	--option FI=${fwhm} \
	--option PS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--option VA=2 \
	--option TH=3 \
	--out=${daoopt} ${fits}
    ${ALLPHOT} daophot find ${fits}
    echo  > ${phoopt} "A1=$(echo ${fwhm} | awk '{print $1*1.2}')"
    echo >> ${phoopt} "IS=$(echo ${fwhm} | awk '{print $1*3}')"
    echo >> ${phoopt} "OS=$(echo ${fwhm} | awk '{print $1*6}')"
    ${ALLPHOT} daophot phot --rad=${phoopt} ${fits}
    ${ALLPHOT} allstar --opt=${alsopt} ${fits}
    ${ALLPHOT} daophot opt --opt=${daoopt} ${im}s.fits
    ${ALLPHOT} daophot find ${im}s.fits
    ${ALLPHOT} daophot offset \
	--id=$(sort -nk1 ${im}.coo | tail -n 1 | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    ${ALLPHOT} daophot phot --rad=${phoopt} --in=${im}s.off ${im}s.fits
    ${ALLPHOT} allstar --opt=${alsopt} --psf=${im}.psf --in=${im}s.ap ${im}s.fits
    ${ALLPHOT} daophot append --out=${im}.cmb ${im}s.als ${im}.als
    ${ALLPHOT} daophot sort --renum --index=4 --out=${im}.all ${im}.cmb
    rm -f ${im}.cmb

    set +e
}


psf_make_() {
    # 5 rounds of psf
    ${ALLPHOT} daophot opt --opt=${im}.opt ${im}s.fits
    for i in $(seq 5); do
	echo " === PSF Iteration $i"
	${ALLPHOT} daophot substar --in=${im}.nei --keep=${im}.lst ${fits}
	${ALLPHOT} daophot psf \
	    --in=${im}.lst \
	    --pho=${im}.all \
	    --out=${im}.psf \
	    ${im}s.fits
	${ALLPHOT} cat neighbours ${im}.nei ${im}.lst
    done

    # 2 rounds of allstar 
    ${ALLPHOT} allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--in=${im}.all ${fits}
    ${ALLPHOT} daophot find ${im}s.fits
    ${ALLPHOT} daophot offset \
	--id=$(tail -n 1 ${im}.all | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    ${ALLPHOT} daophot phot ${radii} --in=${im}s.off ${im}s.fits
    ${ALLPHOT} allstar --psf=${im}.psf --in=${im}s.ap ${im}s.fits
    ${ALLPHOT} daophot append --out=${im}.cmb ${im}s.als ${im}.als
    ${ALLPHOT} allstar --in=${im}.cmb ${fits}
    ${ALLPHOT} daophot sort --renum --index=4 --out=${im}.all ${im}.als
    ${ALLPHOT} daophot find ${im}s.fits
    ${ALLPHOT} daophot offset \
	--id=$(sort -nk1 ${im}.all | tail -n 1 | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    ${ALLPHOT} daophot phot ${radii} --in=${im}s.off ${im}s.fits
    ${ALLPHOT} allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--psf=${im}.psf --in=${im}s.ap ${im}s.fits
    ${ALLPHOT} daophot append --out=${im}.cmb ${im}s.als ${im}.all
    ${ALLPHOT} daophot sort --renum --index=4 --out=${im}.als ${im}.cmb
    # another five rounds of psf
    ${ALLPHOT} daophot pick --nstars=300 --magfaint=${maxmag} ${im}.als
    for i in $(seq 5); do
	echo " === PSF Iteration $i"
	${ALLPHOT} daophot substar --in=${im}.nei --keep=${im}.lst ${fits}
	${ALLPHOT} daophot psf \
	    --in=${im}.lst \
	    --pho=${im}.als \
	    --out=${im}.psf \
	    ${im}s.fits
	${ALLPHOT} cat neighbours ${im}.nei ${im}.lst
    done
    rm -f ${im}.nei
}


# iter_psf_allstar <fits file>
# iterate between PSF and ALLSTAR to create a good PSF and star list
iter_psf_allstar() {
    local fits=${1}
    local im=$(basename ${fits%.*})
    
    set -e

    echo " ========================================================"
    echo " === STEP 1: Make analytical PSF with high S/N stars     "
    echo " ========================================================"

    ${ALLPHOT} daophot opt --dict="${DICTFILE}" --out=${im}.opt ${fits}
    ${ALLPHOT} daophot find --option TH=15 ${fits}
    ${ALLPHOT} daophot phot ${fits} > pho.log
    local maxmag="$(awk '/Estimated/ {print $6-$8*4}' pho.log)"
    rm pho.log
    ${ALLPHOT} daophot pick --nstars=50 --magfaint=${maxmag} ${im}.ap
    ${ALLPHOT} daophot psf \
	--option VA=-1 \
	--option AN=-3 ${fits}
    ${ALLPHOT} cat neighbours ${im}.nei ${im}.lst    
    ${ALLPHOT} daophot psf \
	--option VA=-1 \
	--option AN=$(psf_profile ${im}.psf) \
	${fits}

    ${ALLPHOT} allstar \
	--option WA=0 \
	--option IS=0 \
	--option OS=0 ${fits}


    echo " ================================================================"
    echo " === STEP 2: Remove neighbours and choose best analytical profile"
    echo " ================================================================"

    ${ALLPHOT} daophot sort --index=3 ${im}.als
    ${ALLPHOT} daophot substar --in=${im}.srt --keep=${im}.lst ${fits}
    ${ALLPHOT} daophot opt --opt=${im}.opt ${im}s.fits
    ${ALLPHOT} daophot psf \
	--option VA=-1 \
	--option AN=-7 \
	--pho=${im}.ap \
	--in=${im}.lst \
	--out=${im}.psf \
	${im}s.fits
    rm -f ${im}.srt ${ALLPHOT_PROCDIR}/${im}.srt

    echo " ================================================================"
    echo " === STEP 3: Increase spatial variabilty with one PSF/ALLSTAR run"
    echo " ================================================================"

    local fwhm=$(psf_fwhm ${im}.psf)
    ${ALLPHOT} daophot opt \
	--dict="${DICTFILE}" \
	--option AN=$(psf_profile ${im}.psf) \
	--option FW=${fwhm} \
	--option FI=${fwhm} \
	--option PS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--option VA=2 \
	--option TH=3 \
	--out=${im}.opt ${fits}
    ${ALLPHOT} daophot find ${fits}
    local radii="--radius A1=$(echo ${fwhm} | awk '{print $1*1.2}')"
    radii="${radii} --radius IS=$(echo ${fwhm} | awk '{print $1*3}')"
    radii="${radii} --radius OS=$(echo ${fwhm} | awk '{print $1*6}')"
    ${ALLPHOT} daophot phot ${radii} ${fits}
    ${ALLPHOT} allstar ${fits}
    ${ALLPHOT} daophot opt --opt=${im}.opt ${im}s.fits
    ${ALLPHOT} daophot find ${im}s.fits
    ${ALLPHOT} daophot offset \
	--id=$(sort -nk1 ${im}.coo | tail -n 1 | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    ${ALLPHOT} daophot phot ${radii} --in=${im}s.off ${im}s.fits
    ${ALLPHOT} allstar --psf=${im}.psf --in=${im}s.ap ${im}s.fits
    ${ALLPHOT} daophot append --out=${im}.cmb ${im}s.als ${im}.als
    ${ALLPHOT} daophot sort --renum --index=4 --out=${im}.all ${im}.cmb
    rm -f ${im}.cmb

    echo " ================================================================"
    echo " === STEP 4: Iterate between ALLSTAR and PSF to get unpolluted PSF"
    echo " ================================================================"

    # 5 rounds of psf
    ${ALLPHOT} daophot opt --opt=${im}.opt ${im}s.fits
    for i in $(seq 5); do
	echo " === PSF Iteration $i"
	${ALLPHOT} daophot substar --in=${im}.nei --keep=${im}.lst ${fits}
	${ALLPHOT} daophot psf \
	    --in=${im}.lst \
	    --pho=${im}.all \
	    --out=${im}.psf \
	    ${im}s.fits
	${ALLPHOT} cat neighbours ${im}.nei ${im}.lst
    done

    # 2 rounds of allstar 
    ${ALLPHOT} allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--in=${im}.all ${fits}
    ${ALLPHOT} daophot find ${im}s.fits
    ${ALLPHOT} daophot offset \
	--id=$(tail -n 1 ${im}.all | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    ${ALLPHOT} daophot phot ${radii} --in=${im}s.off ${im}s.fits
    ${ALLPHOT} allstar --psf=${im}.psf --in=${im}s.ap ${im}s.fits
    ${ALLPHOT} daophot append --out=${im}.cmb ${im}s.als ${im}.als
    ${ALLPHOT} allstar --in=${im}.cmb ${fits}
    ${ALLPHOT} daophot sort --renum --index=4 --out=${im}.all ${im}.als
    ${ALLPHOT} daophot find ${im}s.fits
    ${ALLPHOT} daophot offset \
	--id=$(sort -nk1 ${im}.all | tail -n 1 | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    ${ALLPHOT} daophot phot ${radii} --in=${im}s.off ${im}s.fits
    ${ALLPHOT} allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--psf=${im}.psf --in=${im}s.ap ${im}s.fits
    ${ALLPHOT} daophot append --out=${im}.cmb ${im}s.als ${im}.all
    ${ALLPHOT} daophot sort --renum --index=4 --out=${im}.als ${im}.cmb
    ${ALLPHOT} daophot pick --nstars=300 --magfaint=${maxmag} ${im}.als
    for i in $(seq 5); do
	echo " === PSF Iteration $i"
	${ALLPHOT} daophot substar --in=${im}.nei --keep=${im}.lst ${fits}
	${ALLPHOT} daophot psf \
	    --in=${im}.lst \
	    --pho=${im}.als \
	    --out=${im}.psf \
	    ${im}s.fits
	${ALLPHOT} cat neighbours ${im}.nei ${im}.lst
    done
    rm -f ${im}.nei

    echo " ================================================================"
    echo " === STEP 5: Final ALLSTARs with stable PSF with every star      "
    echo " ================================================================"

    ${ALLPHOT} allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--in=${im}.als \
	--out=${im}.all ${fits}
    mv ${im}.all ${im}.als

    ${ALLPHOT} daophot find ${im}s.fits
    ${ALLPHOT} daophot offset \
	--id=$(sort -nk1 ${im}.als | tail -n 1 | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    ${ALLPHOT} daophot phot ${radii} --in=${im}s.off ${im}s.fits
    ${ALLPHOT} allstar \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--psf=${im}.psf --in=${im}s.ap ${im}s.fits
    ${ALLPHOT} daophot append --out=${im}.cmb ${im}s.als ${im}.als
    ${ALLPHOT} allstar --in=${im}.cmb ${fits}
    cp -f ${ALLPHOT_PROCDIR}/*.opt .
    rm -f ${im}.cmb ${im}s.{off,als,ap,coo} ${im}ss.fits
    rm -rf ${ALLPHOT_PROCDIR} 
    unset ALLPHOT_PROCDIR
    set +e
}

usage() {
    echo "$(basename ${0}) [--dict=<dictfile>] <FITS file>...<FITS file>"
    exit
}

DICTFILE=""
ALLPHOT=allphot

[[ $# -lt 1 ]] && usage

while [[ $# -gt 0 ]]; do
    case "${1}" in
	--dict=*) DICTFILE="${1##*=}" ;;
	--debug) ALLPHOT="allphot --debug" ;;
	--help) usage ;;
	--*) echo "Unrecognized option: ${1}" 1>&2 ; exit ;;
        *) break ;;
    esac
    shift
done

for f in $@; do
    export ALLPHOT_PROCDIR=process_${im}
    iter_psf_allstar ${f}
done
