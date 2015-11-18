PRO JDCNV2,YR,MN,DAY,HR,JULIAN,reduced=reduced
;+
; NAME:
;       JDCNV2
; PURPOSE:
;       Converts Gregorian dates to Julian days   
;          (same as JDCNV, but allows keyword to return reduced Julian dates
;           i.e., JD - 2400000.
; CALLING SEQUENCE:
;       JDCNV2,YR,MN,DAY,HR,JULIAN,reduced=reduced
; INPUTS:
;       YR = Year (integer)  
;       MN = Month (integer 1-12)
;       DAY = Day  (integer 1-31) 
;       HR  = Hours and fractions of hours of universal time (U.T.)
;       reduced = keyword telling whether to return Julian date (=0, default)
;                 or reduced Julian dates (=1)
; OUTPUTS:
;       JULIAN = Julian date (double precision) or reduced Julian date
; EXAMPLE:
;       To find the Julian Date at 1978 January 1, 0h (U.T.)
;       JDCNV2,1978,1,1,0.,JULIAN
;       will give JULIAN = 2443509.5   while
;       JDCNV2,1978,1,1,0.,JULIAN,red=1
;       will give JULIAN = 43509.5
;      
; NOTES:
;       (1) JDCNV2 will accept vector arguments 
;       (2) JDCNV and JULDATE are alternate procedures
; REVISON HISTORY:
;       Converted to IDL from Don Yeomans Comet Ephemeris Generator,
;       B. Pfarr, STX, 6/15/88
;       adapted from jdcnv 3/4/92 (GAR)
;-
if n_params(0) lt 4 then begin
	print,string(7B),'CALLING SEQUENCE: JDCNV2,YR,MN,DAY,HR,JULIAN'+$
                         ',reduced=reduced'
        return
endif
if (n_elements(reduced) eq 0) then reduced = 0
yr = long(yr) & mn = long(mn) &  day = long(day)	;Make sure integral
L = (mn-14)/12		;In leap years, -1 for Jan, Feb, else 0
julian = day - 32075l + 1461l*(yr+4800l+L)/4 + $
         367l*(mn - 2-L*12)/12 - 3*((yr+4900l+L)/100)/4 - reduced*2400000L
julian = double(julian) + (HR/24.0D) - 0.5D
return
end          ;jdcnv2
