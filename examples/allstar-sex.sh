#!/bin/bash

make_psf_als() {
    local im=$(basename ${1%.*})
    
    # 1) make fully analytic psf with high S/N stars
    allphot daophot opt --dict=${dict} --out=${im}.opt ${1}
    allphot catalog sex ${1}
    allphot daophot pick --nstars=50 --magfaint=13 ${im}.sex
    allphot daophot psf \
	--option VA=-1 \
	--option AN=1 ${1}
    allphot filters neighbours ${im}.nei ${im}.lst

    # 2)  find best analytical profile on subtracted PSF neighbours image
    allphot allstar do \
	--option WA=0 \
	--option IS=0 \
	--option OS=0 ${1}
    allphot daophot sort --index=3 ${im}.als
    allphot daophot substar --in=${im}.srt --keep=${im}.lst ${1}
    allphot daophot opt --in=${im}.opt ${im}s.fits
    allphot daophot psf \
	--option VA=-1 \
	--option AN=-7 \
	--pho=${im}.ap \
	--in=${im}.lst \
	--nei=${im}.nei \
	--out=${im}.psf \
	${im}s.fits

    # 3) detect all faint stars on residuals
    allphot allstar do ${1}
    local fwhm=$(get_psf_fwhm ${im}.psf)
    allphot daophot opt \
	--dict=${dict} \
	--option FW=${fwhm} \
	--option TH=3 \
	--out=${im}.opt ${1}
    allphot daophot opt --in=${im}.opt ${im}s.fits 
    allphot daophot find ${im}s.fits
    allphot daophot offset \
	--id=$(tail -n 1 ${im}.coo | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    allphot daophot phot --in=${im}s.off ${im}s.fits
    allphot allstar do --psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot daophot sort --renum --index=4 --out=${im}.all ${im}.cmb

    # 4) Make psf fully variable with best profile and subtracted neighbours
    allphot daophot opt --in=${im}.opt \
	--option AN=$(get_psf_profile ${im}.psf) \
	--option FI=${fwhm} \
	--option PS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--option VA=2 \
	${im}s.fits
    allphot daophot pick --magfaint=15 --nstars=200 ${im}.all
    for i in $(seq 1 5); do
	allphot daophot substar --in=${im}.nei --keep=${im}.lst ${1}
	allphot daophot psf --in=${im}.lst --pho=${im}.all --out=${im}.psf --nei=${im}.nei ${im}s.fits
	allphot filters neighbours ${im}.{nei,lst}
    done
    allphot allstar do 	\
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--in=${im}.all ${1}

    allphot daophot find ${im}s.fits
    allphot daophot offset \
	--id=$(tail -n 1 ${im}.als | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    allphot daophot phot --in=${im}s.off ${im}s.fits
    allphot allstar do --psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot allstar do --in=${im}.cmb ${1}

    allphot daophot find ${im}s.fits
    allphot daophot offset \
	--id=$(tail -n 1 ${im}.coo | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    allphot daophot phot --in=${im}s.off ${im}s.fits
    allphot allstar do \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot daophot sort --renum --index=4 --out=${im}.all ${im}.cmb
    allphot daophot pick --magfaint=15 --nstars=200 ${im}.all
    for i in $(seq 1 5); do
	allphot daophot substar --in=${im}.nei --keep=${im}.lst ${1}
	allphot daophot psf --in=${im}.lst --pho=${im}.all --out=${im}.psf --nei=${im}.nei ${im}s.fits
	allphot filters neighbours ${im}.nei ${im}.lst
    done
    allphot allstar do 	\
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--in=${im}.all ${1}
    allphot daophot find ${im}s.fits
    allphot daophot offset \
	--id=$(tail -n 1 ${im}.als | awk '{print $1+1}') \
	--out=${im}s.off ${im}s.coo
    allphot daophot phot --in=${im}s.off ${im}s.fits
    allphot allstar do \
	--option FI=${fwhm} \
	--option IS="$(echo ${fwhm} | awk '{print $1*0.7}')" \
	--option OS="$(echo ${fwhm} | awk '{print $1*4}')" \
	--psf=${im}.psf --in=${im}s.ap ${im}s.fits
    allphot daophot append --out=${im}.cmb ${im}s.als ${im}.als
    allphot allstar do --in=${im}.cmb ${1}
}

dict=""
while [[ $# -gt 0 ]]; do
    case "${1}" in
	--dict=*) dict="${1##*=}" ;;
	--*) echo "Unrecognized option '$1'"; exit ;;
        *) break ;;
    esac
    shift
done

for i in $@; do
    make_psf_als ${i}
done
