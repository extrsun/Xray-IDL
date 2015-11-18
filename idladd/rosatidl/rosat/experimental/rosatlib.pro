PRO ROSATLIB,DUM
;+
; NAME
;    ROSATLIB
; PURPOSE:
;    Add the system variables used for Rosat data analysis
;    Also adds the system variables in use in the IDL Astronomy User's Library
; CALLING SEQUENCE:
;    ROSATLIB
; INPUTS:
;    None.
; OUTPUTS:
;    None.
; METHOD:
;    Calls ASTROLIB from the IDL AUL.
; REVISION HISTORY:
;    Written, Wayne Landsman, July 1986.
;    Use DEFSYSV instead of ADDSYSVAR           December 1990
;-
;
; now do setup for Rosat routines
;
defsysv,'!device',0
defsysv,'!block',0
defsysv,'!docdir',getenv('ZDOC')
defsysv,'!defdir',getenv('ZDEF')
defsysv,'!imapdir',getenv('ZIMAP')
defsysv,'!igrfdir',getenv('ZAUX')
defsysv,'!group',intarr(34,2)
defsysv,'!ebnds',fltarr(34,2)
MESSAGE,'Rosat Library system variables have been added',/INF
;
; Now add variables for Astronomical Users' Library
;
ON_ERROR,2
astrolib
;
;
; Now add physical and astronomical constants 
;
ON_ERROR,2
physcons
;
RETURN
END         ;pro rosatlib
