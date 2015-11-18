PRO PHYSCONS
;+
; NAME
;    PHYSCONSTANT
; PURPOSE:
;    Add physical constants used in calculation
; CALLING SEQUENCE:
;    physcons
; INPUTS:
;    None.
; OUTPUTS:
;    None.
; METHOD:
;    Calls PHYSCONS from the IDL AUL.
; REVISION HISTORY:
;    Written, Wei Sun, August 2011.
;-
;

; constants are from Galactic Astronomy (Binney & Merrifield)
defsysv,'!mp',1.672649D*1.e-24 ;Proton mass
defsysv,'!me',9.10953D*1.e-28  ;Electron mass
defsysv,'!e',4.80320425D*1.e-10 ;Elementary charge
defsysv,'!G',6.672D*1.e-8      ;Gravitational constant
defsysv,'!cl',2.99792458D*1.e10 ;Speed of light in cm/s
defsysv,'!h',6.62618D*1.e-27   ;Planck's constant
defsysv,'!k',1.38066D*1.e-16   ;Boltzman's constant
defsysv,'!ke',0.0861733        ;Boltzman's constant in keV/MK unit
defsysv,'!kev',1.602189D*1.e-9 ;Electron volt in keV
defsysv,'!re',2.81794D*1e-13   ;Electron radius
defsysv,'!sb',5.6703D*1.e-5    ;Stefan-Boltzman constant
defsysv,'!ecs',6.65245D*1.e-25 ;Thomson cross-section in cm^2
defsysv,'!ry',2*!dpi^2*(!e)^4*!me/(!h)^3/!cl ;Rydberg constant
defsysv,'!a0',!h^2/(4*!dpi^2*!me*!e^2) ;Bohn radius
defsysv,'!alf',2*!dpi*!e^2/!cl/!h ;fine-structure constant

defsysv,'!yr',3.1558149984D*1.e7 ;One year in second
defsysv,'!pc',3.08567802D*1.e18 ;Parsec in cm
defsysv,'!msolar',1.989D*1.e33 ;Solar mass in gram
defsysv,'!rsolar',6.9599D*1.e10 ;Solar radius in cm
defsysv,'!lsolar',3.826D*1.e33 ;Solar luminosity in erg/s
MESSAGE,'Physical and astronomical constants have been added',/INF

RETURN
END         ;pro physcons
