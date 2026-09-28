# Fab Build Scripts for Jules

This directory contains the files for building Jules with Fab. It
needs at least Fab version 2.3.0. For the new feature to do
checkout and build separately, current `main` from the Fab repo
is needed.


## Building
The build script is a Python script that relies on Fab.
Jules can be compiled by providing the required
command line options to the script.

Please read the Fab documentation (esp the 
[introduction to the Fab base class](https://metoffice.github.io/fab/fab_base/index.html)
for details. See also the next section ('setup') if you need site-specific
modifications.

The actual build is defined by command line parameters to the
``fab_jules.py`` script. Use the ``-h`` option to list all available
options. Many options are inherited from the Fab base class, only
Jules specific options are defined in ``fab_jules.py`` and are grouped
at the end of the help output::

    Jules configuration options:
      --revision REVISION, -r REVISION
                            Sets the Jules revision to checkout (only used if--checkout is used). Defaults to 'vn7.8'. (default: vn7.8)
      --checkout            If specified, will checkout jules from git or svn, otherwise this script is excepted to be in a cloned git version. (default: False)
      --rivers              If specified, the stand-alone rivers binary will be compiled. (default: False)
      --ascii-out           If specified, NetCDF will be disabled and output will be in ASCII instead. (default: False)


In the ``site_specific`` directory are various site-specific
setups. The main one is called ``default``, and it contains setting
for any compiler currently supported by Fab. But each site can
modify the settings. Have a look at the existing configurations
already contained in Jules, and check the
[Fab documentation](https://metoffice.github.io/fab/fab_base/config.html)
for a full explanation of the available options. 

An example build can be done as follows::

    ./fab_jules.py --site nci --platform gadi --suite gnu  --profile debug

The compilers are selected by specifying a suite (Fab supports out of
the box ``gnu``, ``intel-classic``, ``intel-llvm``, ``nvidia`` and
``cray``), and it will use the corresponding Fortran and C compiler.
If MPI is enabled (which is the default), Fab will search for
corresponding ``mpif90`` and ``mpicc`` compiler wrapper, and verify
that they are indeed of the right suite. If you need to use say
a different C compiler (e.g. use ``gcc`` in an otherwise Intel build),
use the ``-cc`` command line option.

Any Fab script also supports the ``--available-compilers`` flag, which
just lists all compilers that Fab knows about that are available on the
system. Example output (of a system that has gfortran and mpif90 as a
wrapper for gfortran)::

    ----- Available compiler and linkers -----
    Gcc - gcc: gcc
    Mpicc - mpicc-gcc: mpicc
    Gfortran - gfortran: gfortran
    Mpif90 - mpif90-gfortran: mpif90
    Linker - linker-gcc: gcc
    Linker - linker-gfortran: gfortran
    Linker - linker-mpif90-gfortran: mpif90
    Linker - linker-mpicc-gcc: mpicc

The Fab workspace defaults to ``./fab-workspace``, but this can be
changed using the ``--fab-workspace`` command line option.

If the build finished successfully, the binary will be in a directory
like ``fab-workspace/jules-vn7.8-mpi-openmp-debug-mpif90-gfortran/``, it is
called ``jules``. The actual directory name will depend on the
options specified of course.

## Setting up site-specific options
If you need site-specific options (e.g. you want to change the
default compiler flags used for your compiler), create
a directory with the name of your site and platform under
``site-specific``. Please check the existing
[Fab documentation](https://metoffice.github.io/fab/fab_base/config.html)
for examples on setting up options, or the existing
site-specific setups under ``fab/site-specific``.

## Separating checkout and build
The current Fab development version supports the command line options
``--skip-checkout`` and ``--checkout-only``. This allows to run
the checkout separately from the build (e.g. checkout on a machine that
has internet access, but the build on a compute node without external
internet access).
