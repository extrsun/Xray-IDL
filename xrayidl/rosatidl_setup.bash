#! /bin/bash 
#! /bin/csh -x
# Rosat startup script       wqd 7/95; a modified version of JEF  7/92
# Usage: source rosatidl_setup [too]
# where tool is for running idltool
# Assuming that source $rosatdir/rosatidl_setup has been defined in .schrc
# One can, for example, use
# rosatidl tool
#
export RSI_DIR=/usr/local/rsi
export IDL_DIR=/usr/local/rsi/idl
export IDL_PATH=\+~wqd/rosat:\+/usr/local/idladd/idlastronlib/pro:\+/usr/local/idladd/rosatidl:\+$IDL_DIR/lib:\+$IDL_DIR/example:\+~wqd/idladd:\+/data3/yaqoob:\+/data3/andy/ascaarf
alias -x idl=$IDL_DIR/bin/idl
alias -x idlde=$IDL_DIR/bin/idlde 
alias -x idldemo=$IDL_DIR/bin/idldemo
alias -x idldeclient=$IDL_DIR/bin/idldeclient
alias -x idlhelp= $IDL_DIR/bin/idlhelp
alias -x idlrpc=$IDL_DIR/bin/idlrpc
alias -x insight=$IDL_DIR/bin/insight

export ASTRO_DATA=/usr/local/idlastronlib/data

export IDL_STARTUP=~wqd/rosat/rosatsysv.pro

#setenv ZDOC ~wqd/rosat/doc
export ZDEF=~wqd/rosat/def
export ZAUX=~wqd/rosat/aux
export EXPLIB=~wqd/rosat/experimental
#
if [ $# -gt 0 ]; then
idl${1}
else
idl
fi 
#
export -n IDL_DIR
export -n IDL_PATH
export -n IDL_STARTUP
export -n ASTRO_DATA
export -n EXPLIB
