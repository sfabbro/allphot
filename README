I am ready to let you start playing with the latest generation of my software. 
It should compile and run with equal ease under either Unix or Linux.  By
accepting this software you agree to inform me ASAP of any bugs you find, or
improvements that you would like to see.  The documentation has not yet been
fixed.  I will mention below most of the most significant changes in the usage
of the software, but if you would like to volunteer to rewrite sections of the
manuals, or at least point out to me those paragraphs that you find most
misleading and in need of modification, I would appreciate it tremendously.

Any program which deals with images (daophot, allstar, allframe, montage2) can
now accept both IRAF format (.imh and .pix) and FITS format images.  Those
programs which deal with multiple images (allframe, montage2) can deal with a
mixed bag of IRAF and FITS images as input.  To use this dual capability, you
must have both IMFORT libraries .AND. Pence's FITSIO libraries installed on your
system.  If for some reason you don't want the dual capability and refuse to
install one or the other library packages on your computer(s), let me know and I
will send you a reduced-capability version of the critical file, `bothsubs.f'.

To compile the software under Unix, rename `Makefile.unix' to `Makefile'; to
compile the software under Linux, rename `Makefile.linux' to `Makefile'.  You
may have to edit the file to correct the library paths.  Then proceed normally.

WHEN `MAKE'ING THE PROGRAMS THAT DEAL WITH IMAGES, you can change the
array sizes to deal with larger or smaller data sets by editing the
PARAMETER statements in the source code, as before.  However, the parameter
which governs the NUMBER OF IMAGES that can be handled is in the file
`arrays.inc':

      integer maxpic
      parameter (maxpic=900)
      character pictur(maxpic)*30, dattyp(maxpic)*4, imgtyp(maxpic)*4
      integer imid(maxpic), ncol(maxpic), nrow(maxpic)
      common /strings/ pictur, imgtyp, dattyp
      common /values/ imid, ncol, nrow

FOR daophot AND allstar:  maxpic = 2

FOR allframe:  maxpic = 3 times the number of input images (to allow for
               the filenamej and filenamek scratch images)

FOR montage2:  maxpic = the number of input images PLUS 1 (to allow for the
               output image)

WHENEVER YOU MODIFY arrays.inc YOU MUST

% touch bothsubs.f

so that the change gets ingested into all the subroutines.  You must also

% touch `mainprogram'.f 

if you haven't just edited it to modify some of the other array sizes.  IF YOU
FORGET THESE touch'S, YOU'LL PROBABLY GET segmentation faultS AT RUN TIME.

If you use any of my programs BESIDES

daophot allstar daomatch daomaster allframe montage2

let me know what they are and I'll send you compatible copies.
                        
All the software can now be kept in a single subdirectory, unlike the previous
case where (daophot, allstar), (daomatch, daomaster), and (allframe, montage2)
had to be kept segregated.  Just remember the business with arrays.inc
whenever you go to `make' something.

================================================================================

General changes:

In most cases where one of my programs asks for input in the format

                   This is a prompt: <you type something here>

you were able to enter an END-OF-FILE character (^D or, occasionally, ^Z)
to exit the program or retrace to a previous prompt or something.  It
turns out that Linux treats END-OF-FILE characters differently from Unix,
in a way that crapped up what I was trying to do.  Therefore, in place
of the `^D' character, when prompted for something you can now type in
the single character `E' or `e' followed by CARRIAGE-RETURN and reproduce the
effect that the END-OF-FILE used to produce.  If for some reason you want
to name a file `E' or `e', you'll have to type in a filename extension or
rename the file manually outside my software.

The format of output files has been changed slightly to allow for 7-digit
star ID numbers (yes, some of those Eyetalians are doing million-star
reductions).  If you have your own programs that ingest my output files,
watch out for this.

================================================================================

daophot:

The major change here is that when you use the `photometry' aperture-photometry
routine on some image `XXX', the software will look to see whether a file
`XXX.psf' exists in the same directory.  IF SUCH A FILE EXISTS, the routine will
then ask for a file with profile-fitting results (a .als or .alf file, for
instance).  It will also ask for an input star list.  If you have identified a
profile-fitting file (or have accepted the DEFAULT), `photometry' will 

- use the profile-fitting results to subtract every star from the image;
- one by one, 
  - identify each star in the input star list, on the basis of the ID number,
  - add that star back into the image,
  - perform the concentric-aperture photometry,
  - subtract the star from the image again.

Thus the aperture photometry for EACH star in the input list is carried out
with all other known stars in the field subtracted from the image.  It is
up to the user to ensure that (a) the star numbering in the profile-fitting
file and the input star list are identical, and (b) star ID numbers are
not duplicated.  If you want concentric-aperture photometry for all the
stars in the field, the profile-fitting file and the input star list can
be the same file.  More likely, at this stage you just want aperture 
corrections for the image, and will probably perform the aperture photometry
on the same stars that were used to generate the PSF in the first place
(the .lst file).

If the file `XXX.psf' does not exist, the routine will behave as before.
If the file `XXX.psf' DOES exist and for some reason you want to do the
aperture photometry with all the stars still in the image, enter an
END-OF-FILE character (`E' or `e' under Unix or Linux, or `^D' under
Unix) when it asks for the file with the profile-fitting results.

================================================================================

daomatch and daomaster:

daomatch now does not require the user to decide whether the number of
matching stars is sufficient; it will decide for itself when it is
satisfied the provisional transformations are valid.  This is not
foolproof, and very rare mistakes are possible.  You should make a
point of seeing whether the transformations are reasonable, i.e.
whether the scale terms are close to unity if the image is being
transformed to a master image taken with the same camera.  If you do
catch the program making an error, perhaps you can help it out with
one of these new features.

--------------------------------------------------------------------------------

When daomatch asks for the FIRST input file

                                 Master input file:

instead of typing in, for instance

                                 Master input file: filename.als

you append some special characters to the filename

                                 Master input file: filename.als?##

where I am using `?' as a dummy character to stand for one of these:

`%', `<', `[' `|', or `*'

and ## represents some number decided upon by you.  The effects are

filename.als%##  means `consider only stars with CHI < ##' (CHI
                 being the 8th column of a .pk, .nst, .als or .alf file)

filename.als<##  means `consider only stars with instrumental magnitude
                 less than (brighter than) ##'

filename.als[##  means `consider only stars with sharpness index 
                 (column 9) in the interval -## to +##

filename.als|##  means `consider only stars with magnitude uncertainty
                 (column 5) less than ##'

filename.als*    (not followed by a number) means `consider only stars
                 contained within a rectangular subarea'; the program
                 with then prompt for xmin, xmax and ymin, ymax.

The `*' option is especially useful if you have looked at the images
on your monitor and seen that there is only a small part of image 2
that overlaps with a small part of image 1.  If you specify the `*'
option, daomatch will subsequently ask for x limits and y limits
on EACH subsequent input file.

These special features can be combined in any order, but there
must be no embedded spaces in what you type.  That is,

filename.als%2.5[0.75*

means the same as

filename.als*[0.75%2.5 or filename.als[0.75*%2.5

but in

filename.als *[0.75%2.5 or filename.als* [0.75%2.5 or filename.als*[0.75 %2,5 
whatever follows the blank will be missed.

--------------------------------------------------------------------------------

When daomatch asks for SUBSEQUENT input files, 

                                   Next input file:

you can append the following special characters:

`!', `*', `;', '/', or `?'

filename.als!  means `turn off the subarray prompts', i.e. if you want to
               compare the WHOLE of image N with a SUBARRAY of image 1,
               you can use the `*' option on the master list and turn it off
               here so you don't have to keep entering xmin, xmax and
               ymin, ymax

filename.als*  means `turn on the subarray prompts', i.e., if you want to
               compare a SUBARRAY of image N with the WHOLE of image 1, you
               don't specify `*' with the master file name, you specify it
               here

filename.als;  means `this image has the same scale and rotation as the
               master image---look for shifts in x and y only'

'/' can be used either without or with a number ##

filename.als/  means `this image has the same scale as the master image---
               look for shifts and rotations only'

filename.als/##  means `I know the relative scales of the two images---
                 the ratio is ##'; e.g., you know from the respective
                 instrument manuals that the master image (image 1) has
                 a scale of 0.33 arcseconds per pixel, and that image N
                 has a scale of 0.66 arcseconds per pixel.  The scaling
                 factor is 2:  the same two stars are separated by twice
                 as many pixels in image 1 as in image N.  You would then
                 enter

filename.als/2 or filename.als/2.0

                 and only transformations consistent with that scale factor
                 would be considered

filename.als?  means `turn off the `;' or `/' feature---consider transformations
               with ANY shift, rotation, and scale factor.

--------------------------------------------------------------------------------

daomaster also supports the `%', `<', `[', and `|' features if you want to
filter garbage from the input files.  Simply append the special character(s) and
number(s) ## to the filename when it asks for

                            File with list of input files:

HOWEVER, if you do this, daomaster will NOT let you produce the transit table
(.tfr file) or simply transfer the star IDs (.mtr files) because filtering the
input messes up its internal bookkeeping.

--------------------------------------------------------------------------------
