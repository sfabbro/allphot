#!/usr/bin/env python

import sys
import os
import commands
import random
import numpy
from math import *

def generate_sky(skylist, nimages):
    magmin = 17.
    magmax = 27.
    minfwhm = 0.5
    maxfwhm = 1.2
    minmb = 18.
    maxmb = 27.
    dither = 10.
    mingain = 1.
    maxgain = 3.
    minexpo = 100
    maxexpo = 600
    minron = 2.
    maxron = 12.
    minzp =  25.
    maxzp = 27.
    dat = numpy.loadtxt(skylist)
    nstars = dat[:,0].size
    fc = open('skyprocess.sh', 'w')
    for i in range(nimages):
        dx = random.uniform(-dither,dither)
        dy = random.uniform(-dither,dither)
        skylisti = "sky%02d.cat" % i
        fs = open(skylisti, 'w')
        for j in range(nstars):
            line = "100 %10.3f %10.3f %10.3f\n" % (dat[j,1] + dx, dat[j,2] + dy, dat[j,3])
            fs.write(line)
        fs.close()
        cmd = 'sky -c sky.conf '  \
            + skylisti \
            + ' -IMAGE_NAME sky%02d.fits' % i \
            + ' -GAIN %3.1f' % random.uniform(mingain, maxgain) \
            + ' -READOUT_NOISE %3.1f' % random.uniform(minron, maxron) \
            + ' -EXPOSURE_TIME %5.1f' % random.uniform(minexpo,maxexpo) \
            + ' -MAG_ZEROPOINT %5.2f' % random.uniform(minzp, maxzp) \
            + ' -SEEING_FWHM %5.3f' % random.uniform(minfwhm, maxfwhm) \
            + ' -BACK_MAG %7.3f ' % random.uniform(minmb, maxmb) \
            + ' -STARCOUNT_ZP 0'
        fc.write(cmd + '\n')
    fs.close()

if __name__ == '__main__':
    #if len(sys.argv)==1 :
    #    print "%s nstars nimages" % os.path.basename(sys.argv[0])    
    #generate_sky(sys.argv[1], sys.argv[2])
    generate_sky('sky.list', 4)

