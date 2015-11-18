pro glactc_m,ra,dec,year,gl,gb,j
;+
;   Name:
;      GLACTC
;   Purpose:
;      Program to convert right ascension (ra) and declination (dec) to
;      galactic longitude (gl) and latitude (gb) (j=1) or vice versa (j=2).
;   Calling Sequence:
;      glactc,ra,dec,year,gl,gb,j
;   Inputs:
;      year     equinox of ra and dec       (input)
;      j        direction of conversion     (input)
;               1:  ra,dec --> gl,gb
;               2:  gl,gb  --> ra,dec
;   Inputs or Outputs, depending on argument J:
;      ra       right ascension, hours
;      dec      declination, degrees
;      gl       galactic longitude (system II), degrees
;      gb       galactic latitude (system II), degrees
;      All results forced double precision floating.
;   Common blocks:
;      gal      See Side Effects.    
;   Side effects:
;      Year and galaxy orientation saved in common to make repeated     
;      computations more efficient.
;   History:
;      FORTRAN subroutine by T. A. Nagy, 21-MAR-78.
;      Conversion to IDL, R. S. Hill, STX, 19-OCT-87.
;	Modified by WQD for converting arrays
;-
common gal,oldyr,rapol,decpol,dlon,sdp,cdp,radhrs
if n_params(0) lt 6 then begin
     print,'CALLING SEQUENCE: glactc,ra,dec,year,gl,gb,j
     print,'j = 1: ra,dec --> gl,gb   j = 2:  gl,gb -->ra,dec
     return
endif
sz = size(oldyr) 
first = sz(1) eq 0 
new = 0
if not first then new=(double(year) ne oldyr)
if first or new then begin
   oldyr = double(year)
   ;
   ; Galactic pole at ra 12 hrs 49 mins, dec 27.4 deg, equinox 1950.0
   ; position angle from galactic center to equatorial pole = 123 degs.
   rapol = 12.0d0 + 49.0d0/60.0d0 + 8.13d-4 * (year-1950.0d0)
   decpol = 27.4d0 - 5.44d-3 * (year-1950.0d0)
   dlon = 123.0d0 - 1.33d-3 * (year-1950.0d0)
   sdp = sin(decpol/!radeg)
   cdp = sqrt(1.0d0-sdp*sdp)
   radhrs=!radeg/15.0d0
endif
;
; Branch to required type of conversion.
case j of                   
    1:  begin
        sdec = sin(dec/!radeg)
        cdec = sqrt(1.0d0-sdec*sdec)
        sgb = sdec*sdp + cdec*cdp*cos((ra-rapol)/(radhrs))
        gb = !radeg * asin(sgb)
        cgb = sqrt(1.0d0-sgb*sgb)
        sine = cdec * sin((ra-rapol)/radhrs) / cgb
        cose = (sdec-sdp*sgb) / (cdp*cgb)
        gl = dlon - !radeg*atan(sine,cose)

	c=where(gl lt 0.,nc)
	if nc ne 0 then gl(c)=gl(c)+360.0d0
;        if gl lt 0.0 then gl=gl+360.0d0
        return
        end
    2:  begin
        sgb = sin(gb/!radeg)
        cgb = sqrt(1.0d0-sgb*sgb)
        sdec = sgb*sdp + cgb*cdp*cos((dlon-gl)/!radeg)
        dec = !radeg * asin(sdec)
        cdec = sqrt(1.0d0-sdec*sdec)
        sinf = cgb * sin((dlon-gl)/!radeg) / cdec
        cosf = (sgb-sdp*sdec) / (cdp*cdec)
        ra = rapol + radhrs*atan(sinf,cosf)
	c=where(ra ge 24.0,nc)
	if nc ne 0 then ra(c) = ra(c) -24.0d0
;        if ra ge 24.0 then ra = ra-24.0d0
        return
        end
endcase
end
