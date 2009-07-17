# -*-bash-*-
#!/usr/bin/env bash
#
#DAOPHOT_TRANS_DIR=@DATADIR@
#DAOPHOT_MASTER_OPTION_FILE=@SYSCONFDIR@/@PACKAGE@.conf
#
#
export ALLPHOT_TRANS_DIR=$(readlink -f $(dirname $0 ) )
export ALLPHOT_OPTION_FILE=/home/cadc/fabbros/src/allphot/scripts/allphot.conf

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

# option_get_masterfile [<master option file>]
option_get_masterfile() {
    local optfile=${1:-${ALLPHOT_OPTION_FILE}}
    [[ ! -r ${optfile} ]] && optfile=${ALLPHOT_OPTION_FILE}
    if [[ ! -r ${optfile} ]]; then
	echo "*** Error: could not find a valid allphot option file" >&2
	return
    fi
    echo ${optfile}
}

# option_get_value <option name> [<master option file>]
option_get_value() {
    local value=$(grep "\b${1}.*=" $(option_get_masterfile ${2}) | cut -d '=' -f 2 | cut -d '#' -f 1)
    if [[ -z ${value} ]]; then
	echo "*** Error: option ${1} unknown" >&2
	return
    fi
    echo ${value}
}

# option_change_value <option name>=<new option value> <master option file>
option_change_value() {
    sed -i -e "s/\b${1%=*}=.*/${1}/" ${2} || \
	echo "*** Error: could not change option ${1} in ${2}" >&2
}

# option_make_daofile <daophot|photo|allstar|allframe> [<master option file> <dao option file (default <program>.opt)>]
option_make_daofile() {
    local tmpoptfile=${1}.tmp.opt dopt= mopt=
    rm -f ${tmpoptfile}
    while read line; do
	dopt=$(cut -f1 -d '=' <<< "${line}")
	mopt=$(option_get_value $(cut -f2 -d '=' <<< "${line}") ${2})
	[[ -z ${mopt} ]] && break
	echo "${dopt}=${mopt}" >> ${tmpoptfile}
    done < ${ALLPHOT_TRANS_DIR}/${1}.tbl
    # mv file if non empty lines
    [[ $(sed '/^\s*$/d' ${tmpoptfile} | wc -l) -gt 4 ]] && mv -f ${tmpoptfile} ${3:-${1}.opt}
}

# option_synchronize_daofiles [<master option file>]
option_synchronize_daofiles() {
    local progs="daophot photo allstar allframe"
    local optfile=$(option_get_masterfile ${1})
    for i in ${progs}; do
	option_make_daofile ${i} ${optfile}
    done
}

# option_update_from_fits <FITS file> <FITS assoc table> [<master option file>]
option_update_from_fits() {
    local fkey= mopt= fval=
    local optfile=$(option_get_masterfile ${3})
    while read line; do
	fkey=$(cut -f1 -d '=' <<< "${line}")
	mopt=$(cut -f2 -d '=' <<< "${line}")
	fval=$(fits_get_key ${fkey} ${1})
	[[ -n ${fval} ]] && \
	    option_change_value ${mopt}=${fval} ${optfile}
    done < ${2}
}

# option_update_from_fwhm <fwhm> [<master option file>]
option_update_from_fwhm() {
    local fwhm=${1}
    [[ -z ${fwhm} ]] && return
    local optfile=$(option_get_masterfile ${2})
    option_change_value FIND_FWHM=${fwhm} ${optfile}
    option_change_value PSF_RADIUS=$(calc "${fwhm}*4.5") ${optfile}
    option_change_value PROFIT_RADIUS=$(calc "${fwhm}*1.4") ${optfile}
    option_change_value PROFIT_INNER_SKY=$(calc "${fwhm}*0.5") ${optfile}
    option_change_value PROFIT_OUTER_SKY=$(calc "${fwhm}*7") ${optfile}
     for i in $(seq 3); do
	option_change_value A${i}=$(calc "${fwhm}*${i}*0.8") ${optfile}
    done
    option_change_value PHOT_INNER_SKY=$(calc "${fwhm}*3") ${optfile}
    option_change_value PHOT_OUTER_SKY=$(calc "${fwhm}*3.5+15") ${optfile}
}

option_update_photo_from_fwhm() {
    local fwhm=${1}
    [[ -z ${fwhm} ]] && return
    local optfile=$(option_get_masterfile ${3})
    for i in $(seq 9); do
	option_change_value A${i}=$(calc "${fwhm}*${i}*0.8") ${optfile}
    done
    option_change_value AA=$(calc "${fwhm}*10*0.8") ${optfile}
    option_change_value AB=$(calc "${fwhm}*11*0.8") ${optfile}
    option_change_value AC=$(calc "${fwhm}*12*0.8") ${optfile}
    option_change_value PHOT_INNER_SKY=$(calc "${fwhm}*12") ${optfile}
    option_change_value PHOT_OUTER_SKY=$(calc "${fwhm}*12+20") ${optfile}
}

option_update_psf_model() {
    local optfile=$(option_get_masterfile ${1})    
    option_change_value PSF_MODEL=-6 ${optfile}
    option_change_value PSF_VARIABILITY=2 ${optfile}
    option_change_value PICK_NSTARS=120 ${optfile}
}
