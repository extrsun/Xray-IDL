;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;	rspsfh
;
;*PURPOSE:
;  Calculate point spread function (normalized surface brightness)
;    for Rosat HRI
;
;*CALLING SEQUENCE:
;	rspsfh, offcen, offang, psf, ierr=ierr, chatter=chatter
;
;*PARAMETERS:
;  OFFCEN  - Off axis angle of (center of) PSF (in arcmin)
;  OFFANG  - off axis angle = angle from target position (arcsec)
;  CHATTER - controls program feedback to user (default = 1)
;
;*OUTPUTS:
;  IERR  - contains codes for error conditions:
;          0 = no error, 1 = ?
;  PSF   - surface brightness of point spread function, normalized so that  
;          Integral 2*PI*r*dr*f   from 0 to infinity = 1   [1/arcsec2]
;
;*RESTRICTIONS:
;
;*NOTES:
;  Calculates energy averaged response.
;  Uses functional form as of 27 Mar 1992, derived by MPE using data
;    from HZ 43, AR Lac, and LMC X-1:
;
;  PSF = A1 * exp( -0.5 * (R/S1) ** 2 ) + A2 * exp( -0.5 * (R/S2) ** 2 )
;             + A3 * exp( -R/S3 ),
;
;     with R = radial distance (arc sec), and
;              A1 = 0.9638     S1 =  2.1858
;              A2 = 0.1798     S2 =  4.0419
;              A3 = 0.00090     S3 = 31.69
;
; The off-axis parameterization is accomodated by modification of S2 as
; follows:
;
;   S2=3.3 + 0.019*theta - 0.016(theta**2) + 0.0044(theta**3)
;
; where theta is the off-axis angle in arcmin.
;
; NB: this expression has not been forced to reproduce the on-axis value. The
; fact that the off-axis expression gives S2=3.3 for theta=0, compared to the
; on-axis equation which gives 4.0, reflects the variation in aspect quality
; from observation to observation.
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    adapted  16 Aug 1992 from code in FAQ.hri.
;    modified 03 Mar 1994 (GAR) to include off-axis parameterization. Value 
;      of a3 also changed from 0.00116751D0 to 0.0009.
;-
;-------------------------------------------------------------------------------
pro rspsfh,offcen,offang,psf,ierr=ierr,chatter=chatter
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSPSFH, offcen (arcmin), offang (arcsec), PSF (1/arcsec^2),'
  print,'         IERR = IERR, chatter = chatter (1)'
  return
endif
if (n_elements(chatter) eq 0) then chatter = 1        ;default is 1
;
pi = 3.14159
efac = alog(10.)
ierr = 0
;
; coefficients for psf, derived by MPE by combining data from
; HZ 43, AR Lac, and LMC X-1
;
a1 =  0.96379391D0
a2 =  0.17975701D0
a3 =  0.00090D0
s1 =  2.18575551D0
s2 =  4.04192030D0
if (offcen ne 0) then $
  s2 = 3.3 + 0.019*offcen - 0.016*offcen*offcen + 0.0044*offcen*offcen*offcen
s3 = 31.68980665D0
;
arg1 = 0.5*(offang/s1)*(offang/s1)
arg2 = 0.5*(offang/s2)*(offang/s2)
arg3 = offang/s3
;
; calculate point spread function
;
psf = a1*exp(-arg1) + a2*exp(-arg2) + a3*exp(-arg3)
;
return
end     ;pro rspsfh
