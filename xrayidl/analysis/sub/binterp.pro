PRO BINTERP, XTAB, YTAB, ZTAB, XINT, YINT, ZINT,grid=grid
;+
; NAME:   
;	LINTERP  
; PURPOSE: 
;	Linearly interpolate tabulated data from one data grid to another.
;
; CALLING SEQUENCE:
;	LINTERP, Xtab, Ytab, Xint, Yint   
;
; INPUT PARAMETERS: 
;	Xtab -  Vector containing the current independent variable grid.
;		Must be monotonic increasing or decreasing
;	Ytab -  Vector containing the current dependent variable values at 
;		the XTAB grid points.
;	Xint -  Scalar or vector containing the new independent variable grid 
;		points for which interpolated value(s) of the dependent 
;		variable are sought.
;
; OUTPUT PARAMETERS:
;	Yint  -  Scalar or vector with the interpolated value(s) of the 
;		dependent variable at the XINT grid points.
;		YINT is double precision if XTAB or YTAB are double,
;		otherwise YINT is REAL*4
;
; EXAMPLE:
;	To linearly interpolate from an IUE spectrum wavelength grid
;	WAVE, FLUX to another grid defined as:
;	WGRID = [1540., 1541., 1542., 1543., 1544, 1545.]
;   
;	IDL>  LINTERP, WAVE, FLUX, WGRID, FGRID  
;
;	FGRID will be a 6 element vector containing the values of FLUX 
;	linearly interpolated onto the WGRID wavelength scale
;
; PROCEDURE: 
;	Uses TABINV to calculate the effective index of the values
;	in XINT in the table XTAB.  The resulting index is used
;	to calculate the interpolated values YINT from the values
;	in YTAB.  If XINT is outside the range of XTAB, then it is truncated
;	to the end value.
;
; NOTES:
;	Users with IDL versions before V2.2.2 need to replace the call to
;	INTERPOLATE with the commented lines at the end of the procedure
;
; PROCEDURES CALLED:
;	TABINV, ZPARCHECK
; MODIFICATION HISTORY:
;	Adapted from the IUE RDAF,  W. Landsman      October, 1988
;	Modified to use the new INTERPOLATE function        June, 1992
;	Modified to always return REAL*4             October, 1992
; 	Modified by wqd for interpolating a grid.
;-
; On_error,2

 if N_params() LT 4 then begin
   print,'Syntax - LINTERP, Xtab, Ytab, Xint, Yint' 
   print,'    Xtab, Ytab - Input X and Y vectors
   print,'    Xint - Input X value (scalar or vector) at which to interpolate
   print,'    Yint - Output interpolated Y value
   return
 endif
;if keyword_set(grid) eq 0 then grid=0
; zparcheck, 'LINTERP', xtab, 1, [1,2,3,4,5], 1, 'Current X Vector' 
; zparcheck, 'LINTERP', ytab, 2, [1,2,3,4,5], 1, 'Current Y Vector' 
; zparcheck, 'LINTERP', xint, 3, [1,2,3,4,5], [0,1], 'New X Vector or Scalar'
; commented out because they don't allow a vector like temp(1,30).

; Determine index of data-points from which interpolation is made

 tabinv, xtab, xint, xr                                    
 tabinv, ytab, yint, yr                                    

 s = size( ztab)

; Perform interpolation

if keyword_set(grid) eq 0 then begin
	zint=xint*0. ; keep the dimension type 
 if s(s(0)+1) LE 3 then  $                  ;Integer or byte input?
     zint(0) = interpolate( float(ztab), xr,yr) else $
     zint(0) = interpolate( ztab, xr,yr)
endif else begin
 if s(s(0)+1) LE 3 then  $                  ;Integer or byte input?
     zint = interpolate( float(ztab), xr,yr,/grid) else $
     zint = interpolate( ztab, xr,yr,/grid)
endelse
 return
 end                                        
