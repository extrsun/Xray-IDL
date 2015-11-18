;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   FILLBOX
;
;*PURPOSE:
; A procedure to fill in a rectangle whose endpoints are specified by
; the input X & Y vectors. 
; Used in FILLSIMPLE and FILLREGION
; The resulting integer vectors include the X and Y values of all pixels
; contained within the rectangle.
;
;*CALLING SEQUENCE:
;   FILLBOX,xendpt,yendpt,XPIXEL,YPIXEL, XMIN, XMAX, YMIN, YMAX
;
;*INPUT:
;   XENDPT - An IDL variable (vector or array), usually floating or double
;            Must be between -32767.5 and 32767.5 to avoid integer overflow
;            Defines the X values of the boundary endpoints
;   YENDPT - similar to X, but defines the Y values of the boundary endpoints
;
;*OUTPUT:
;   XPIXEL - The integer X values of all pixels within the region
;   YPIXEL - The integer Y values of all pixels within the region
;   XMIN, XMAX - Minimum and maximum values of x position
;   YMIN, YMAX - Minimum and maximum values of y position
;
;*MODIFICATION HISTORY:
;   written 13 July 1991 by GAR
;   modified 15 Apr 1992 to fix a bug when xmin, xmax, ymin, ymax are
;     integers already (GAR)
;-
pro fillbox,xendpt,yendpt,xpixel,ypixel,xmin,xmax,ymin,ymax
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'FILLBOX, xendpt, yendpt, XPIXEL, YPIXEL, XMIN, XMAX, YMIN, YMAX'
  retall
endif
;
; find the bounds of the box defined by xendpt and yendpt, and the
; numbers of pixels along x & y
;
xmin = min(xendpt)
if ((xmin - fix(xmin)) eq 0) then nxmin = xmin else $
   nxmin = fix( xmin - (xmin lt 0) )      ;next lower integer
xmax = max(xendpt)
if ((xmax - fix(xmax)) eq 0) then nxmax = xmax else $
   nxmax = fix( xmax + (xmax gt 0) )      ;next highest integer
ymin = min(yendpt)
if ((ymin - fix(ymin)) eq 0) then nymin = ymin else $
   nymin = fix( ymin - (ymin lt 0) )      ;next lower integer
ymax = max(yendpt)
if ((ymax - fix(ymax)) eq 0) then nymax = ymax else $
   nymax = fix( ymax + (ymax gt 0) )      ;next highest integer
;
nxpix = nxmax - nxmin + 1 
nypix = nymax - nymin + 1
xpix = indgen(nxpix) + nxmin           ;range of integers including xendpt
ypix = indgen(nypix) + nymin           ;range of integers including yendpt
npix = long(nxpix)*long(nypix)
;
; fill in the x and y positions of all of the pixels within the box
; 
xpixel = intarr(npix)                  ;first, fill up the rectangle
ypixel = intarr(npix)                  ;which contains xendpt & yendpt
for ii=0,nypix-1 do begin 
  ict = long(ii)*long(nxpix)
  xpixel(ict) = xpix 
  ypixel(ict) = intarr(nxpix)+ypix(ii) 
endfor
;
return          ;pro fillbox
end
