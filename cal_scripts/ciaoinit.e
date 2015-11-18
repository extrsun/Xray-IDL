 ########################################################################
 # startup script for software packages installed at /net/xray/usr/local/

 # alias for the CIAO package:
 setenv NETDIR /net1/hobby
 setenv LOCALDIR $NETDIR/usr/local
 set version=3.0.1
 setenv CALDB  $LOCALDIR/ciao_${version}/CALDB
  setenv CALDBCONFIG $CALDB/software/tools/caldb.config
  setenv CALDBALIAS $CALDB/software/tools/alias_config.fits
  alias ciao "source  $LOCALDIR/ciao_${version}/bin/ciao_users.csh"
  set path = ($LOCALDIR/ciao_${version}/contrib $path)
  set path = ($LOCALDIR/ciao_${version}/contrib/interpreted $path)
 #setenv CALDB  $LOCALDIR/ciao_2.3
 #setenv CALDBCONFIG $CALDB/software/tools/caldb.config
 #setenv CALDBALIAS $CALDB/software/tools/alias_config.fits
 #alias ciao "source  $LOCALDIR/ciao_2.3/bin/ciao_users.csh"
 #alias ciao "source  $LOCALDIR/ciao_2.3/bin/ciao.csh"
 #set path = ($LOCALDIR/ciao_2.3/contrib $path)
 #set path = ($LOCALDIR/ciao_2.3/contrib/interpreted $path)

 # location of FV (the "Fits Viewer"):
 setenv FV  $LOCALDIR/fv2.6/linux/	
 set path = ($FV/bin $path)

 # to run XANADU (incl. XSpec,  FTools, etc.):
 #setenv LHEASOFT  $LOCALDIR/lheasoft/Linux_2.2_i686
 #source $LHEASOFT/lhea-init.csh
 setenv LHEASOFT $NETDIR/software/lheasoft/Linux_2.4_i686
 source $LHEASOFT/lhea-init.csh

 # alias for SAOImage DS9:

 alias ds9 " $LOCALDIR/ds9"
 alias newds9 " $LOCALDIR/newds9/ds9"

#for model acisabs:
setenv LMODDIR $NETDIR/software/acisabs
setenv LD_LIBRARY_PATH "${LMODDIR}:$LD_LIBRARY_PATH"

 # end startup script
 ########################################################################
