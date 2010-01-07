# psf analytical profile 
prof=1
# name of the dictionary 
field=sky

for i in sky*.fits; do
    im=${i%.*}
    allphot daophot opt --dict=${field}.dict ${im}.fits
    allphot daophot psf ${im}.fits
    allphot filters neighbours daophot_${im%.*}/${im}.{nei,lst}
    allphot daophot psf --prof=${prof} ${im}.fits
    allphot filters neighbours daophot_${im%.*}/${im}.{nei,lst}
    allphot daophot psf --var=2 ${im}.fits
    allphot daophot allstar ${im}.fits
done
