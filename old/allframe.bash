# -*-bash-*-
#!/usr/bin/env bash

source ${ALLPHOT_EXEC_DIR}/options.bash

# daophot_match <reference catalogue> <mch file> <list of catalogs files>
daophot_match() {
    # Master input file
    echo ${1}
    # Output file name
    echo ${2}
    # Next input files
    cat ${3}
    echo
    echo y
}

# daomaster_standard <field name>
daophot_master() {
    local field=${1%.*}
    local niter=10 # matchup radii iterations
    local nframes=$(wc -l ${field}.mch | cut -d ' ' -f 1)
    awk '{print "rm -f "substr($1,2,match($1,/\./)-2)".{coo,mtr}"}' ${field}.mch | sh
    rm -f ${field}.{mag,cor,raw,tfr}
    # start answering boring questions
    echo ${field}.mch
    echo -n "$(option_get_value MS daomaster.opt), "
    echo -n "$(option_get_value MC daomaster.opt), "
    echo "$(option_get_value MF daomaster.opt)"
    option_get_value MD daomaster.opt
    option_get_value GE daomaster.opt
    option_get_value MR daomaster.opt
    for i in $(seq ${nframes}); do echo ; done
    for i in $(seq ${niter}); do option_get_value MI daomaster.opt; done
    echo 0
    #  Assign new star IDs? 
    echo y
    #  A file with mean magnitudes and scatter? 
    echo y
    echo ${field}.mag
    # A file with corrected magnitudes and errors?
    echo y
    echo ${field}.cor
    # A file with raw magnitudes and errors?
    echo y
    echo ${field}.raw
    # A file with the new transformations?
    echo y
    echo ${field}
    # default overwrite
    echo
    # A file with the transfer table?
    echo y
    echo ${field}.tfr
    # Individual .COO files?
    echo y
    # Simply transfer star IDs?
    echo y
}

# daophot_allframe <mch file> <mag file>
daophot_allframe() {
    # assume allframe.opt correct
    echo
    # file with initial matches
    echo ${1}
    # file with all stars to fit
    echo ${2}
}
