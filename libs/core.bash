# -*-bash-*-

# check_do function args
# Check that function exists, and call it with args.
check_do() {
    local function="${1}"
    shift
    if is_function "${function}" ; then
	${function} "$@"
    else
	die "No function ${function}"
    fi
}

# die [-q] "Message" PUBLIC
# Display "Message" as an error. If -q is not provided, gives a stacktrace.
die() {
    local item funcname="" sourcefile="" lineno="" n errfun dostack="yes"
    
    # do we have a working write_error_msg?
    if is_function "write_error_msg" ; then
	errfun="write_error_msg"
    else
	errfun="echo"
    fi
    
    # quiet?
    if [[ ${1} == "-q" ]] ; then
	dostack=""
	shift
    fi

    ${errfun} "${@:-(no message)}"

    if [[ -n "${dostack}" ]] ; then
	echo "Call stack:" 1>&2
	for (( n = 1 ; n < ${#FUNCNAME[@]} ; ++n )) ; do
	    funcname=${FUNCNAME[${n}]}
	    sourcefile=$(basename ${BASH_SOURCE[${n}]})
	    lineno=${BASH_LINENO[$(( n - 1 ))]}
	    echo "    * ${funcname} (${sourcefile}:${lineno})" 1>&2
	done
    fi
	
    # Evil, but effective.
    kill ${ALLPHOT_KILL_TARGET}
    exit 249
}

# do_action action args...
# Load and do 'action' with the specified args
do_action() {
    local action="${1##--}" modfile="" subaction="${2##--}"
    [[ -z ${action} ]] && die "Usage: do_action <action> <args>"
    shift; shift
    
    ALLPHOT_ACTION_NAME="${action}"
    ALLPHOT_COMMAND="${ALLPHOT_PROGRAM_NAME} ${ALLPHOT_MODULE_NAME}"
    
    [[ ${ALLPHOT_BINARY_NAME##*/} != ${ALLPHOT_PROGRAM_NAME} ]] && \
	ALLPHOT_COMMAND="${ALLPHOT_BINARY_NAME##*/}"
    
    modfile=$( allphot_find_module "${action}" )
    (
	source "${ALLPHOT_DEFAULT_ACTIONS}" 2>/dev/null \
	    || die "Couldn't source ${ALLPHOT_DEFAULT_ACTIONS}"
	source "${modfile}" 2>/dev/null \
	    || die "Couldn't source ${modfile}"
	if [[ -z ${subaction} ]] ; then
	    check_do "do_${DEFAULT_ACTION:-usage}" "$@"
	else
	    is_function "do_${subaction}" \
		|| die -q "Action ${subaction} unknown"
	    check_do "do_${subaction}" "$@"
	fi
   )
}

# inherit module PUBLIC
# Sources a given eselect library file
inherit() {
    local x
    for x in "$@"; do
	[[ -e "${ALLPHOT_CORE_PATH}/${x}.bash" ]] \
	    || die "Couldn't find ${x}.bash"
	source "${ALLPHOT_CORE_PATH}/${x}.bash" \
	    || die "Couldn't source ${x}.bash"
    done
}

# GNU sed wrapper (real path to GNU sed determined by configure)
sed() {
    /bin/sed "$@"
}
