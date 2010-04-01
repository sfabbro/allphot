im=${1%.*}

listhead ${im}.fits 
}
fits_get_key() {
    [[ ! -r ${2} ]] && die -q "fits_get_key: FITS file ${2} not found"
    local value=$(listhead ${2} 2> /dev/null \
	| grep "^${1}.*=" \
	| cut -d '=' -f 2 \
	| awk '{print $1}' \
	| sed -e "s:'::g")
    [[ -z ${value} ]] && die -q "fits_get_key: FITS keyword ${1} not found"
    echo ${value}
}

listhead sky01.fits | awk 'match($0,"^IMATYPE[[:space:]]*=[[:space:]]*(.*)[[:space:]]*/[[:space:]].*$",a)

zp  = fitskey(MAGZERO) + 2.5*log10(fitskey(EXPOTIME));
sky = pow(10, 0.4*(zp + 2.5*log10(fitskey(PIXSIZE)**2) - fitskey(MAGBACK)));
rms = sqrt(fitskey(GAIN) * sky + fitskey(RON)**2) / fitskey(GAIN);
fwhm= fitskey(SEEING) * fitskey(PIXSIZE);
lobad=sky - 50 * rms;
thresh=4 * rms;
printf " NL    NX    NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD\n"
printf "  1 %5d %5d %8.1f %8.1f %8.2f %8.2f %8.2f %8.2f %8.2f\n\n" NAXIS1 NAXIS2 lobad SATLEV thresh fwhm GAIN RON fwhm*1.2
printf "%7i %10.3f %10.3f %10.4f %10.4f\n" , ++i, $2, $3, $4, sky;
