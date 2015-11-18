;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   MOI_BOX
;
;*PURPOSE:
; Used in MAKEMOI.
; A procedure to fill in a rectangle whose endpoints are specified by
; the input X & Y limits.
; Skips the region if the limits are outside specified ranges.
;
;*CALLING SEQUENCE:
;   MOI_BOX,xlim1,xlim2,ylim1,ylim2,ixmin,ixmax,iymin,iymax,XPIX,YPIX,SKIP
;
;*INPUT:
;   XLIM1, XLIM2 - the specified minimum & maximum values of X
;   YLIM1, YLIM2 - the specified minimum & maximum values of Y
;   IXMIN, IXMAX - the min & max pixel X positions of the region
;   IYMIN, IYMAX - the min & max pixel Y positions of the region
;
;*OUTPUT:
;   XPIX - The integer X values of all pixels within the rectangle
;   YPIX - The integer Y values of all pixels within the rectangle
;   SKIP - A flag specifying whether the region is within the allowed 
;          ranges (skip = 0) or not (skip = 1)
;
;*PROCEDURES CALLED:
;   FILLBOX
;
;*MODIFICATION HISTORY:
;   written 13 July 1991 by GAR
;-
pro moi_box,xlim1,xlim2,ylim1,ylim2,ixmin,ixmax,iymin,iymax,xpix,ypix,skip
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'MOI_BOX, xlim1, xlim2, ylim1, ylim2, ixmin, ixmax, iymin, iymax,'$
       ,'         XPIX, YPIX, SKIP'
  retall
endif
;
xpix = [-1]
ypix = [-1]
skip = 0
if ( (ixmin gt xlim2) or (ixmax lt xlim1) ) then skip = 1
if ( (iymin gt ylim2) or (iymax lt ylim1) ) then skip = 1
if ( skip eq 0 ) then begin
  xendpt = [ixmax>xlim1,ixmax>xlim1,ixmin<xlim2,ixmin<xlim2]
  yendpt = [iymax<ylim2,iymin>ylim1,iymin>ylim1,iymax<ylim2]
  fillbox,xendpt,yendpt,xpix,ypix
endif
;
return        ;pro moi_box
end
