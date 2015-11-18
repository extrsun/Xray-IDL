
4 October 1993

Dear colleagues,

     The IDL procedure which calculates mean exposure maps for Rosat PSPC
pointed observations is now available. The name of the procedure is
MAKE_EMAP. The code has been tested and appears to work well. However, if
you find any errors and/or bugs, we would certainly like to know about
them! We also welcome sugestions for improvement. 

     You can obtain the procedure files both via DECNET copy from the
HEASRC Vax workstation on the LHEA Vax cluster, and via anonymous ftp from
the GOF account on the Sun workstation legacy.nasa.gsfc.gov. 

I. via DECNET copy from HEASRC:

     The IDL procedures are stored on the HEASRC Vax workstation under the
account IDL:[LIB.ROSAT].  In addition to the IDL Astronomical Users' 
Library FITS routines, you will need the following files:

[LIB.ROSAT.EXPERIMENTAL]*.pro       IDL procedure files (including those
                                    needed for MAKE_EMAP)

[LIB.ROSAT.DEF]make_emap.def        File which defines default parameter values

[LIB.ROSAT.DOC]make_emap.help       Help file which gives details on how to run 
                                    MAKE_EMAP, etc.

CALDB:[DATA.ROSAT.PSPC.CPF]det*.fits    The PSPC instrument maps. You will
                                        need 26,025 blocks of disk space.

     You may also find the following files helpful:

[LIB.COM]idldef.com                 Example of how to set up system logicals

[LIB.COM]startup.pro                Defines IDL system variables when 
                                    starting IDL

II. Via anonymous ftp from the GOF account on legacy.gsfc.nasa.gov
    (128.183.8.233):

     The IDL procedure, online help, parameter default, and auxiliary data
files are stored in an uncompressed tar file in the directory
rosat/software/idl. You may choose to update your entire library
or to copy only those files used by MAKE_EMAP which have been modified
since January 1, 1993. 

     You will need the following files:

rosat/software/idl/rosat_lib.tar       Tar file for the entire library
or
rosat/software/idl/make_emap.tar       Contains only those files used by
                                       MAKE_EMAP which have been changed
                                       since Jan 1, 1993
and
caldb/data/rosat/pspc/cpf/det*.fits    The PSPC instrument maps. You will
                                       need 13.320 Mbytes of disk space.

(All files should be copied in binary mode.)

     After copying the tar files, you will then need to unpack them, e.g.,
 
     tar -xvf rosat_lib.tar        (unpacks tar file and creates 
                                    subdirectory structure)

Unpack rosat_lib.tar from the directory which you wish to be your IDL top
level directory. Unpack make_emap.tar from your rosat/experimental
subdirectory. Then move the files from subdirectory make_emap to their
proper locations: *.pro files to your rosat/experimental subdirectory,
make_emap.def to your rosat/def subdirectory, make_emap.help and 
.idlrc_example to your rosat/doc subdirectory.

III. Running MAKE_EMAP at your home institution:

     The procedure ROSATLIB defines an IDL system variable !imapdir, which is
initially set to the definition of the system logical/environmental variable
ZIMAP upon starting IDL. If you do not change the names of the PSPC 
instrument map files when you copy them, then all you need to do is to change
the definition of !imapdir. You can do this in either of two ways:

     Within IDL, by defining the system variable !imapdir (use this also to
     switch between instrument maps stored in different directories)

     Before entering IDL, by defining the system logical (setting the 
     environmental variable) ZIMAP

You can also use the keyword IMAPDIR in the command statement to accomplish 
the same thing.

     If you do change the names of the PSPC instrument map files when you
copy them, then you will also need to edit the procedure file defimap.pro.

     The help file make_emap.help gives details on how to run MAKE_EMAP, 
etc.
