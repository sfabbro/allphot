#!/bin/sh

if [ $# -lt 1 ]; then
    echo "Usage: $(basename $0) <dict> <fits image 1>...<fits image n>"
fi

# name of the field
field=${1}
shift

rm -f ${field}.list
for i in $*; do    
    im=${i%.*}
    ls -1 daophot_${im}/${im}.als >> ${field}.list
done
ref=$(head -n1 ${field}.list)
sed -i -e '1d' ${field}.list
allframe_process ${field} ${ref}
allphot allframe match --field=${field} ${ref} ${field}.list
allphot allframe master ${field}.mch
allphot allframe allframe ${field}.mch ${field}.mag
