# -*-bash-*-  vim: ft=bash

# has test list
# Return true if list contains test
has() {
    local test=${1} item
    shift
    for item in "$@" ; do
	[[ ${item} == ${test} ]] && return 0
    done
    return 1
}

# is_function function PUBLIC
# Test whether function exists
is_function() {
    [[ $(type -t "${1}" ) == "function" ]]
}

# is_number PUBLIC
# Returns true if and only if $1 is a positive whole number
is_number() {
    [[ -n ${1} ]] && [[ -z ${1//[[:digit:]]} ]]
}
