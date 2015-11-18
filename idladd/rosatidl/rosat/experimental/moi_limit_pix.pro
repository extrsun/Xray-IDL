;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   MOI_BOX
;
;*PURPOSE:
; Used in MAKEMOI.
; A procedure to select out pixels which fall within specified limits
; in X & Y and also within an annulus defined by 2 radii
;
;*CALLING SEQUENCE:
;   MOI_LIMIT_PIX,xpix,ypix,xlim1,xlim2,ylim1,ylim2,x0,y0,radmin,radmax,SKIP
;
;*INPUT:
;   XPIX - values of the X positions of the pixels
;   YPIX - values of the Y positions of the pixels
;   XLIM1, XLIM2 - the specified minimum & maximum values of X
;   YLIM1, YLIM2 - the specified minimum & maximum values of Y
;   X0 - the X center of the annulus
;   Y0 - the Y center of the annulus
;   RADMIN - the inner radius of the annulus
;   RADMAX - the outer radius of the annulus
;
;*OUTPUT:
;   XPIX - The integer X values of all pixels within the specified
;          X & Y limits & annulus
;   YPIX - The integer Y values of all pixels within the specified
;          X & Y limits & annulus
;   SKIP - A flag specifying whether the pixels region is within the 
;          specified limits & annulus (skip = 0) or not (skip = 1)
;
;*MODIFICATION HISTORY:
;   written 13 July 1991 by GAR
;-
pro moi_limit_pix,xpix,ypix,xlim1,xlim2,ylim1,ylim2,x0,y0,radmin,radmax,skip
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'MOI_LIMIT_PIX, xpix, ypix, xlim1, xlim2, ylim1, ylim2, x0, y0,'$
       ,'               radmin, radmax, SKIP'
  retall
endif
;
skip = 0
rad = where( (xpix ge xlim1) and (xpix le xlim2) and (ypix ge ylim1) and $
             (ypix le ylim2) )
if ( rad(0) ge 0 ) then begin
  xpix = xpix(rad)
  ypix = ypix(rad)
  delx = xpix - x0
  dely = ypix - y0
  rad = sqrt( delx*delx + dely*dely )
  rad = where( (rad ge radmin) and (rad le radmax) )
  if (rad(0) ge 0) then begin
    nintreg,xpix(rad),ypix(rad),ix,iy
    xpix = ix
    ypix = iy
  endif else skip = 1
endif else skip = 1
;
return        ;pro moi_limit_pix
end
