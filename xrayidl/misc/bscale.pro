FUNCTION BSCALE,OIMAGE,IMIN,IMAX,ntoppix=ntoppix,nbotpix=nbotpix
;+
; modified version of the original procedure writen by J. Saken
; ntoppix - the number of highest intensity pixels reserved for other uses
; WQD, Oct. 11, 1993
;-
on_error,2
if n_elements(nbotpix) eq 0 then nbotpix=0
if n_elements(ntoppix) eq 0 then ntoppix=0
image=float(oimage)
IF n_elements(imin) eq 0  THEN IMIN=MIN(IMAGE)
IF n_elements(imax) eq 0 THEN IMAX=MAX(IMAGE)
;
JUNK = CHECK_MATH(TRAP=0, 0,1)
if !D.N_COLORS gt 256 then begin
ARRAY = (256-ntoppix-1) * ((IMAGE < IMAX)-IMIN)/(IMAX-IMIN) > nbotpix
;(!D.N_COLORS-ntoppix-1)
endif else $
ARRAY = (!D.N_COLORS-ntoppix-1) * ((IMAGE < IMAX)-IMIN)/(IMAX-IMIN) > nbotpix
;(!D.N_COLORS-ntoppix-1)
JUNK = CHECK_MATH(TRAP=0, 0,1)
RETURN,ARRAY
END













