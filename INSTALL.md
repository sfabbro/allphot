# Installation Instructions

## Requirements

*   **BASH:** Most systems will have this installed already.
*   **C Compiler:** Most systems will have a C compiler like GCC installed.
*   **CFITSIO:** This is a library for reading and writing FITS files. Most Linux distributions provide a package for it (e.g., `libcfitsio-dev` on Debian-based systems).
*   **GNU `readlink` or `realpath`:** These commands are used to find the canonical path of a file. They are usually pre-installed on Linux. On macOS and other BSD-based systems, you may need to install `coreutils` (e.g., via MacPorts or Homebrew).
*   **`bc`:** A command-line calculator. On Debian-based systems, you can install it with `sudo apt-get install bc`.
*   **ALLFRAME:** This is a required runtime dependency for the photometry modules. See the section "The `ALLFRAME` Dependency" in the `README` file for more details on how to obtain and install it.

## Building from Source

This project uses the standard GNU Autotools build system. To build the project, run the following commands from the root of the repository:

```bash
autoreconf -fvi
./configure
make
```

## Testing

To run the test suite, run the following command:

```bash
make check
```

## Installation

To install the project, run the following command. You may need to use `sudo` depending on the installation prefix.

```bash
make install
```
