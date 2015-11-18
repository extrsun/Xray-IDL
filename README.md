# Xray-IDL
Here deposit some additional IDL procedures used for analysing the X-ray data.

Please fork it and set up the system paramters for IDL in the appropriate way. Following is corresponding settings in my laptop (MacOS, C-shell: you need to modify the the "xidl_setup" file if you are under the Borne-again shell):

setenv progdir "/Users/sunwei/ProgramFiles"

setenv PUBDIR $progdir/addition/pub

setenv IDLUTILS_DIR $progdir/itt/idl71/lib/idlutils

setenv IDL_PATH \+${IDL_DIR}/lib:\+/Users/sunwei/data/pro

setenv IDL_PATH ${IDL_PATH}:\+$IDLUTILS_DIR/pro:\+$IDLUTILS_DIR/godadard/pro

alias xidl "source $PUBDIR/xrayidl/xidl_setup.v71"


