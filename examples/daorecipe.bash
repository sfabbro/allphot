daophot_opt GA=1.5 RE=3
daophot_attach ${1}
daophot_find 1,1 ${1%.*}.coo
cat > photo.opt <<EOF
A1=4
IS=12
OS=20
EOF
daophot_phot photo.opt ${1%.*}.coo ${1%.*}.ap
daophot_pick ${1%.*}.ap 30 15 ${1%.*}.lst
daophot_psf ${1%.*}.ap ${1%.*}.lst ${1%.*}.psf
daophot_exit
