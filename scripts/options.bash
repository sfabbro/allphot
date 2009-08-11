# -*-bash-*-
#!/usr/bin/env bash

export ALLPHOT_OPT_DAOPHOT=${ALLPHOT_OPT_DIR}/daophot.opt
export ALLPHOT_OPT_PHOTO=${ALLPHOT_OPT_DIR}/photo.opt
export ALLPHOT_OPT_ALLSTAR=${ALLPHOT_OPT_DIR}/allstar.opt
export ALLPHOT_OPT_ALLFRAME=${ALLPHOT_OPT_DIR}/allframe.opt
export ALLPHOT_OPT_DAOMASTER=${ALLPHOT_OPT_DIR}/daomaster.opt

calc() {
    echo "scale=4; $1" | bc ;
}

# return the value of a FITS keyword
fits_get_key() {
    if [[ ! -r ${2} ]]; then
        echo "*** Error FITS file ${2} not found" >&2
        return
    fi    
    local value=$(listhead ${2} 2> /dev/null | grep "^${1}.*=" | cut -d '=' -f 2 | awk '{print $1}')
    if [[ -z ${value} ]]; then
        echo "*** Error FITS keyword ${1} not found" >&2
        return
    fi
    echo ${value}
}

# option_get_value <option name> <option file>
option_get_value() {
    local value=$(grep "\b${1}.*=" ${2} | cut -d '=' -f 2)
    if [[ -z ${value} ]]; then
	echo "*** Error: option ${1} unknown in ${2}" >&2
	return
    fi
    echo ${value}
}

# option_change_value <option name>=<new option value> <option file>
option_set_value() {
    sed -i -e "s/\b${1%=*}=.*/${1}/" ${2} || \
	echo "*** Error: could not change option ${1} in ${2}" >&2
}

# option_update_from_fits <FITS file> <FITS assoc table>
option_update_from_fits() {
    local fkey= mopt= fval=
    while read line; do
	fkey=$(cut -f1 -d '=' <<< "${line}")
	mopt=$(cut -f2 -d '=' <<< "${line}")
	fval=$(fits_get_key ${fkey} ${1})
	[[ -n ${fval} ]] && \
	    option_set_value ${mopt}=${fval} daophot.opt
    done < ${2}
}

# option_update_from_fwhm <fwhm>
option_update_from_fwhm() {
    local fwhm=${1}
    [[ -z ${fwhm} ]] && return
    option_set_value FW=${fwhm} daophot.opt
    option_set_value PS=$(calc "${fwhm}*4.5") daophot.opt
    option_set_value FI=$(calc "${fwhm}*1.4") daophot.opt
    option_set_value FI=$(calc "${fwhm}*1.4") allstar.opt
    option_set_value IS=$(calc "${fwhm}*0.5") allstar.opt
    option_set_value OS=$(calc "${fwhm}*7")   allstar.opt
}

option_update_photo_from_fwhm() {
    local fwhm=${1}
    [[ -z ${fwhm} ]] && return
    for i in $(seq 9); do
	option_set_value A${i}=$(calc "${fwhm}*${i}*0.8") photo.opt
    done
    option_set_value AA=$(calc "${fwhm}*10*0.8") photo.opt
    option_set_value AB=$(calc "${fwhm}*11*0.8") photo.opt
    option_set_value AC=$(calc "${fwhm}*12*0.8") photo.opt
    option_set_value IS=$(calc "${fwhm}*12") photo.opt
    option_set_value OS=$(calc "${fwhm}*12+20") photo.opt
}

option_update_psf_model() {
    option_set_value AN=-6 daophot.opt
    option_set_value VA=2 daophot.opt
}
