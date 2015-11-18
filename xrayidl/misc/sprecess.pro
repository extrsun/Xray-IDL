pro sprecess, ra, dec, equinox1, equinox2, PRINT = print
;+
; NAME:
;    SPRECESS
; PURPOSE:
;    Precess coordinates from EQUINOX1 to EQUINOX2.  For interactive
;    display, one should use ASTRO which calls PRECESS.
; CALLING SEQUENCE:
;    PRECESS, ra, dec, [ equinox1, equinox2, PRINT = ]
; INPUT - OUTPUT:
;    RA - Input right ascension in DEGREES (scalar or vector)
;    DEC - Input declination in DEGREES (scalar or vector)
;          NOTE: The input RA and DEC are modified by PRECESS to give the 
;                 values after precession.
; OPTIONAL INPUTS:
;    EQUINOX1 - Original equinox of coordinates, numeric scalar.  If omitted, 
;             PRECESS will query for EQUINOX1 and EQUINOX2.
;    EQUINOX2 - Equinox of precessed coordinates.
; OPTIONAL INPUT KEYWORDS:
;    PRINT - If this keyword is set and non-zero, then the precessed
;            coordinates are displayed at the terminal.
; RESTRICTIONS:
;    Accuracy of precession decreases for declination values near 90 
;    degrees.  PRECESS should not be used more than 2.5 centures from
;    1900.    
; EXAMPLE:
;    Precess the 1950 coordinates of Eps Ind (RA = 21h 59m,33.053s,
;    DEC = (-56d, 59', 33.053") to equinox 1975.
;
;    IDL> precess, ten(21,59,33.053)*15., ten(-56,59,33.053),1950,1975,/print
;
; PROCEDURE:
;    Algorithm from Computational Spherical Astronomy by Taff (1983), 
;    p. 24. 
; REVISION HISTORY
;    Written, Wayne Landsman, STI Corporation  August 1986
;    Correct negative output RA values   February 1989
;    Added /PRINT keyword      W. Landsman   November, 1991
;    Exactly the same as the ASTRON precess routine, except single precesion.
;              JMS   Sept. 1992
;-    
;On_error,2                                           ;Return to caller

NPAR = N_params()
CDR = 0.17453292519943e-1
;
if ( npar LT 2 ) then begin 
   print,'CALLING SEQUENCE - precess, ra, dec, [ equinox1, equinox2, PRINT =]
   print,'    NOTE: RA and DEC must be supplied in DEGREES'
   RETURN
endif else if (npar LT 4) then $
   read,'Enter original and new equinox of coordinates: ',equinox1,equinox2 

npts = min( [N_elements(ra), N_elements(dec)] )
if npts EQ 0 then $  
       message,'ERROR - Input RA and DEC must be vectors or scalars'

ra_rad = ra*cdr	  	;Convert to double precision if not already
dec_rad = dec*cdr 
;
a = cos(dec_rad)  

case NPTS of   	         ;Is RA a vector or scalar?
 1:    x = [a*cos(ra_rad), a*sin(ra_rad), sin(dec_rad)] ;input direction cosines
 else: begin  	
      x = fltarr(npts,3)
      x(*,0) = a*cos(ra_rad)
      x(*,1) = a*sin(ra_rad)
      x(*,2) = sin(dec_rad)
      x = transpose(x)
      end
endcase
;
csr = cdr/3600.

t = 0.001*( equinox2 - equinox1)

st = 0.001*( equinox1 - 1900.)
;                                Compute 3 rotation angles

A = CSR*T*(23042.53 + ST*(139.75 +0.06*ST) $
    +T*(30.23 - 0.27*ST+18.0*T))

B = CSR*T*T*(79.27 + 0.66*ST + 0.32*T) + A

C = CSR*T*(20046.85 - ST*(85.33 + 0.37*ST) $
    +T*(-42.67 - 0.37*ST -41.8*T))
;
sina = sin(a) &  sinb = sin(b)  & sinc = sin(c)
cosa = cos(a) &  cosb = cos(b)  & cosc = cos(c)
r = fltarr(3,3)
r(0,0) = [ cosa*cosb*cosc-sina*sinb, sina*cosb+cosa*sinb*cosc,  cosa*sinc]
r(0,1) = [-cosa*sinb-sina*cosb*cosc, cosa*cosb-sina*sinb*cosc, -sina*sinc]
r(0,2) = [-cosb*sinc, -sinb*sinc, cosc]
;
x2 = r#x 	;rotate to get output direction cosines

 if npts EQ 1 then begin	         ;Scalar

	ra_rad = atan(x2(1),x2(0))
	dec_rad = asin(x2(2))

 endif else begin	         ;Vector     

	ra_rad = fltarr(npts) + atan(x2(1,*),x2(0,*))
	dec_rad = fltarr(npts) + asin(x2(2,*))

 endelse

ra = ra_rad/cdr
ra = ra + (ra LT 0.)*360.            ;RA between 0 and 360 degrees
dec = dec_rad/cdr

if keyword_set( PRINT ) then $
   print, 'Equinox ('+strtrim(equinox2,2) + '): ',adstring(ra,dec,1)

return
end
                       

