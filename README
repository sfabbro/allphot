# ALLPHOT - Stellar Photometry Automated

ALLPHOT is a package designed to automate the ALLFRAME stellar photometry package.

ALLPHOT consists of fancy wrapper scripts for all ALLFRAME (DAOPHOT, DAOMATCH, DAOMASTER, ALLFRAME, MONTAGE2) programs to be run unattended. It provides a friendly command-line interface that allows you to write scripts quickly. It has all the original command parameters but with good default values, so the user does not have to deal with thousands of arcane tweaks. It also handles files and directories properly. It does not touch any of the ALLFRAME source code.

ALLPHOT was originally developed with the CANFAR project to re-run photometry on MACHO data in the hope it can be applied to other large data sets. It is now used in other projects as well.

It can be used from other command-line programs, as its API is fairly generic. The code was originally inspired by the "eselect" tool used on Gentoo Linux and shamefully borrows a lot of code from it, but it has diverged from it significantly. It is written in pure bash, with some C tools to deal with FITS files (using cfitsio).

## Availability

The development git repository is public on GitHub: [http://github.com/sfabbro/allphot](http://github.com/sfabbro/allphot)

## Installation

See the `INSTALL` file for installation instructions.

## Documentation

*   All ALLPHOT commands are wrappers around the ALLFRAME commands, so it is very much advised to know how the original ALLFRAME commands work. Refer to the original DAOPHOT and ALLFRAME papers and the documentation included in the ALLFRAME tarball for more details.

*   Typing `allphot <command> --help` (or with no arguments) will describe what you can do with a particular command.

*   All commands are straightforward and intuitive. For example, a typical original and interactive DAOPHOT `FIND` command would be as follows, assuming you have a `daophot.opt` file:

    ```
    $ cat daophot.opt
    GA=1.6
    RE=5.5

    $ daophot
    [...snip...]
     Command: attach image.fits
    [...snip...]
     Command: find
    [...snip...]
               Number of frames averaged, summed: 1,1
          File for positions (default image.coo):
    [...snip...]
		      Are you happy with this? Y
     Command: exit
    ```

    You can get exactly the same result in a single line with ALLPHOT:

    ```
    $ allphot daophot find --option GA=1.6 --option RE=5.5 image.fits
    [...snip...]
    ```

*   There is a very basic dictionary generator to grab some values from a FITS header to help build options files. Example:

    ```
    $ cat image.dict
    GA=fits(GAIN)
    RE=fits(RDNOISE)

    $ allphot daophot option --dict=image.dict image.fits
    ```

*   To get a list of available modules, just run:

    ```
    $ allphot help
    ```

*   The command line for each module is shown by running:

    ```
    $ allphot <module> usage
    ```

*   To get a list of DAOPHOT submodules, run:

    ```
    $ allphot daophot list
    ```

*   Modules have default arguments that you can overwrite with command-line options.

*   There are also some example scripts in the `examples/` directory.

*   Two tools to parse FITS headers are also compiled:
    *   `fitshead`: print a FITS header from a HDU or all of them
    *   `fitskey`: print or modify keys of FITS headers
    *   `fitssatur`: automatic determination of a saturation level

## Contributing to ALLPHOT

If you have suggestions, bugs, or are interested in contributing to ALLPHOT development, please send the authors an email (see the `AUTHORS` file) or interact directly with the git repository at: [http://github.com/sfabbro/allphot](http://github.com/sfabbro/allphot)
