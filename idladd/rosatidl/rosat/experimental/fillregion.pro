;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   FILLREGION
;
;*PURPOSE:
; A procedure to fill in a 2D region whose endpoints are specified by
; the input X & Y vectors. The endpoints are connected by straight lines
; to define the boundaries of the region.
; The resulting integer vectors include the X and Y values of all pixels
; contained within the region.
; It is assumed that the endpoints are contiguous.
;
;*CALLING SEQUENCE:
;   FILLREGION,xendpt,yendpt,XPIXEL,YPIXEL
;
;*INPUT:
;   XENDPT - An IDL variable (vectoror array), usually floating or double
;            Must be between -32767.5 and 32767.5 to avoid integer overflow
;            Defines the X values of the boundary endpoints
;   YENDPT - similar to X, but defines the Y values of the boundary endpoints
;
;*OUTPUT:
;   XPIXEL - The integer X values of all pixels within the region
;   YPIXEL - The integer Y values of all pixels within the region
;
;
;*RESTRICTIONS:
;   Input variables must have the same numbers of elements
;   It is assumed that the endpoints are contiguous, i.e. that the command
;        plot,[xendpt,xendpt(0)],[yendpt,yendpt(0)]
;   with !psym=0 will define a simple region.
;   Consecutive endpoints must be different, or no solution is possible
;   and program will quit.
;
;*COMMENTS:
;   FILLREGION can be very slow for regions which contain large numbers of
;   pixels. FILLSIMPLE is much faster, but works only for simple regions
;   (i.e., where all of the vertices point away from the center). If
;   dealing with a large, complicated region, separate the region into 
;   simple subsections and use FILLSIMPLE.
;
;*PROCEDURES CALLED:
;   FILLBOX
;   GETBORDER
;
;*MODIFICATION HISTORY:
;   written 5 July 1991 by GAR
;-
;-------------------------------------------------------------------------------
pro fillregion,xendpt,yendpt,xpixel,ypixel
;
; check the input vertices to make sure x & y have the same numbers of 
; elements, and that each has > 1 element.
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'FILLREGION, xendpt, yendpt, XPIXEL, YPIXEL'
  retall
endif
;
nelx = n_elements(xendpt)
if (nelx le 1) then begin
  print,' Number of elements of XENDPT must be > 1. Returning.'
  retall
endif
;
nely = n_elements(yendpt)
if (nely le 1) then begin
  print,' Number of elements of YENDPT must be > 1. Returning.'
  retall
endif
;
if (nelx ne nely) then begin
  print,' XENDPT and YENDPT must have the same number of elements. Returning.)'
  retall
endif
;
; use fillbox to fill the xpixel and ypixel with the x & y positions
; of all the pixels in the box which contains the region
;
fillbox,xendpt,yendpt,xpixel,ypixel,xmin,xmax,ymin,ymax
nxmin = fix( xmin - (xmin lt 0) )      ;next lower integer
nxmax = fix( xmax + (xmax gt 0) )      ;next highest integer
nymin = fix( ymin - (ymin lt 0) )      ;next lower integer
nymax = fix( ymax + (ymax gt 0) )      ;next highest integer
;  
;  if xendpt & yendpt define a rectangle, then quit
;  otherwise, get the points on the boundary definied by the endpoints
;
if (nelx eq 4) then begin     
  if ( (xendpt(0) eq xendpt(1)) and (xendpt(2) eq xendpt(3)) and $
       (yendpt(1) eq yendpt(2)) and (yendpt(3) eq yendpt(0)) ) then return
  if ( (xendpt(1) eq xendpt(2)) and (xendpt(3) eq xendpt(0)) and $
       (yendpt(0) eq yendpt(1)) and (yendpt(2) eq yendpt(3)) ) then return
endif
getborder,xendpt,yendpt,xborder,yborder
;
;  define the center of the boundary. calculate radius & polar angle theta
;  of the boundary x & y values (bdx & bdy). Order bdx & bdy by theta.
;
nbord = n_elements(xborder)
xcenter = (nxmax + nxmin)/2. + 0.1      ;make sure xcenter is not an integer
ycenter = (nymax + nymin)/2. + 0.1
delx = xborder - xcenter 
dely = yborder - ycenter
;
;  solve for theta. Use delx = bdx - xcen & dely = bdy - ycen to determine
;  quadrant.
;
rad = sqrt( delx*delx + dely*dely )
theta = atan(dely/delx)
ind = where( (delx lt 0) and (dely ge 0) ) 
if (ind(0) ge 0) then theta(ind) = 3.14159 + theta(ind)
ind = where( (delx lt 0) and (dely le 0) ) 
if (ind(0) ge 0) then theta(ind) = 3.14159 + theta(ind)
ind=where( (delx gt 0) and (dely le 0) ) 
if (ind(0) ge 0) then theta(ind) = 3.14159*2. + theta(ind)
;
;  sort boundary x & y in order of increasing theta
;
ind = sort(theta) 
theta = theta(ind) 
rad = rad(ind)
xborder = xborder(ind) 
yborder = yborder(ind)
;
;  do the same thing for the ractangle of points which surrounds the x & y
;  endpoints
;
delx = xpixel - xcenter 
dely = ypixel - ycenter
rad2 = sqrt( delx*delx+dely*dely )
theta2 = atan(dely/delx)
ind = where( (delx lt 0) and (dely ge 0) ) 
if (ind(0) ge 0) then theta2(ind) = 3.14159 + theta2(ind)
ind = where( (delx lt 0) and (dely le 0) ) 
if (ind(0) ge 0) then theta2(ind) = 3.14159 + theta2(ind)
ind = where( (delx gt 0) and (dely le 0) ) 
if (ind(0) ge 0) then theta2(ind) = 3.14159*2. + theta2(ind)
;
savex = xpixel            ;save xpixel & ypixel as these will be redefined
xpixel = savex*0. - 999.
savey = ypixel
ypixel = savey*0. - 999.
;
;  make sure that the boundary contour is closed in theta & radius
;
theta=[0.,theta,2.*3.14159]
rad=[(rad(0)+rad(nbord-1))/2.,rad,(rad(0)+rad(nbord-1))/2.]
;
;  for each wedge in theta, find all pixels whose radii are within the
;  average radius for that wedge (+ 0.5 to be conservative)
;  include those pixels in the new xpixel & ypixel vectors
;
ict = 0
for ii=0,nbord do begin
  ang1 = theta(ii) 
  ang2 = theta(ii+1)
  radsel = (rad(ii)>rad(ii+1)) +.5
  ind = where( (theta2 ge ang1) and (theta2 lt ang2) and (rad2 le radsel) )
  if (ind(0) ge 0) then begin
    xpixel(ict) = nint(savex(ind))
    ypixel(ict) = nint(savey(ind))
    ict = ict + n_elements(ind)
  endif
endfor
ind = where( (xpixel ge 0) and (ypixel ge 0) )
xpixel = xpixel(ind)         ;strip off bogus negative values
ypixel = ypixel(ind)
;
;  make a plot to make sure everything worked, if desired
;
if (!debug gt 2) then begin
  set_xy,xmin,xmax,ymin,ymax   
  !psym=3 
  plot,savex,savey,/ynozero
  !psym=0 
  oplot,[xendpt,xendpt(0)],[yendpt,yendpt(0)]
  !psym=1 
  oplot,xborder,yborder
  !psym=2 
  oplot,xpixel,ypixel & !psym=0
endif
;
return
end          ;pro fillregion
