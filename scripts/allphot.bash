#!/bin/bash
#

source options.bash
source recipes.bash

usage() {
    echo "Usage: allphot [OPTIONS] <command> <fits images>"
}

OPTIND=1
while getopts "h" opt; do
    case "${opt}" in
	h) usage
    esac
done

action=$(basename "${0}")

if [[ -n ${action} ]] ; then
    if is_function "do_${action}" ; then
	do_${action} "$@"
    else
	do_action "${action}" "$@"
    fi
else
    usage
fi
