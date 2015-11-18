;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   GETBORDER
;
;*PURPOSE:
; A procedure to calculate the x and y positions of pixels along a border
; defined by the input X & Y vectors. 
; Used in FILLREGION
; The resulting integer vectors include the X and Y values of all pixels
; along the border.
; It is assumed that the endpoints are contiguous.
;
;*CALLING SEQUENCE:
;   GETBORDER,xendpt,yendpt,XBORDER,YBORDER,FLAG
;
;*INPUT:
;   XENDPT - An IDL variable (vector or array), usually floating or double
;            Must be between -32767.5 and 32767.5 to avoid integer overflow
;            Defines the X values of the boundary endpoints
;   YENDPT - similar to X, but defines the Y values of the boundary endpoints
;
;*OUTPUT:
;   XBORDER - The integer X values of all pixels along the border
;   YBORDER - The integer Y values of all pixels along the border
;   FLAG - a vector containing one element per edge of the border.
;          tells how the pixel positions were determined.
;          if flag = 0 (normal) then the equation y = mx + b was solved.
;          if flag = 1 (x2 = x1) then the equation x = my + b was solved.
;
;*RESTRICTIONS:
;   Input variables must have the same numbers of elements
;   It is assumed that the endpoints are contiguous, i.e. that the command
;        plot,[xendpt,xendpt(0)],[yendpt,yendpt(0)]
;   with !psym=0 will define a simple region.
;   Consecutive endpoints must be different, or no solution is possible
;   and program will quit.
;
;*MODIFICATION HISTORY:
;   written 13 July 1991 by GAR
;-
pro getborder,xendpt,yendpt,xborder,yborder,flag
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'GETBORDER, xendpt, yendpt, XBORDER, YBORDER, FLAG'
  retall
endif
;
; check the input vertices to make sure x & y have the same numbers of 
; elements, and that each has > 1 element.
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
;  Solve for the straight line between each
;  contiguous pair of endpoints - this is the boundary. Define bdx & bdy.
;  flag = 0 means y was solved in terms of x
;  flag = 1 means x was solved in terms of y
;
nend = n_elements(xendpt)
if (nend lt 2) then begin
  print,' Number of endpoints = ',nend,' < 2. Returning.'
  retall & end
;
xmin = min(xendpt)
xmax = max(xendpt)
ymin = min(yendpt)
ymax = max(yendpt)
;
x = xendpt 
y = yendpt
if ( (x(nend-1) ne x(0)) and (y(nend-1) ne y(0)) and $
     (nend gt 2) ) then begin
  x = [xendpt,xendpt(0)] 
  y = [yendpt,yendpt(0)] 
  nend = nend + 1
endif
;
;  solve for straight line joining endpoints, segment by segment
;
flag = bytarr(nend-1)
xborder = -1. 
yborder = -1.
for ii=0,nend-2 do begin      
  x1 = x(ii) 
  x2 = x(ii+1) 
  y1 = y(ii) 
  y2 = y(ii+1)
  delx = x2 - x1 
  dely = y2 - y1
;
;  if delx and dely both = 0, then return with an error message
;
  if ( (delx eq 0) and (dely eq 0) ) then begin
    print,' Endpoints ',ii,' and ',ii+1,' are the same. Returning.'
    retall
  endif
;
;  if delx=0, solve for x as function of y. Otherwise, solve for y.
;
  if (delx eq 0) then begin 
    flag(ii) = ii
    slope = delx/dely
    scale = nint( (ymax-ymin)/abs(dely) )
    ybd = findgen( nint( (ymax-ymin)*scale) )/scale + ymin
    xbd = slope*(ybd-y1) + x1
  endif else begin 
    slope = dely/delx
    scale = nint( (xmax-xmin)/abs(delx) )
    xbd = findgen( nint( (xmax-xmin)*scale) )/scale + xmin
    ybd = slope*(xbd-x1) + y1  
  endelse
;
;  include only those points which are within the rectangle defined by 
;  (x(ii),y(ii)) and (x(ii+1),y(ii+1))
;
  ind = where( (xbd ge (x1<x2)) and (xbd le (x1>x2)) and $
               (ybd ge (y1<y2)) and (ybd le (y1>y2)) )
  if (ind(0) ge 0) then begin 
    xbd = xbd(ind) 
    ybd = ybd(ind) 
  endif
;
  xborder = [xborder,xbd] 
  yborder = [yborder,ybd]
endfor
xborder = xborder(1:*)             ;strip off initial bogus values
yborder = yborder(1:*)         
;
return           ;pro getborder
end
