;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   FILLSIMPLE
;
;*PURPOSE:
; A procedure to fill in a 2D region whose endpoints are specified by
; the input X & Y vectors. 
; Essentially, a faster version of FILLREGION, designed for "simple" regions 
; (i.e., all vertices point *away* from the center).
; The resulting integer vectors include the X and Y values of all pixels
; contained within the region.
; It is assumed that the endpoints are contiguous.
;
;*CALLING SEQUENCE:
;   FILLSIMPLE,bdx,bdy,XPIXEL,YPIXEL
;
;*INPUT:
;   BDX - An IDL variable (vector or array), usually floating or double
;         Must be between -32767.5 and 32767.5 to avoid integer overflow
;         Defines the X values of the boundary endpoints
;   BDY - similar to X, but defines the Y values of the boundary endpoints
;
;*OUTPUT:
;   XPIXEL - The integer X values of all pixels within the region
;   YPIXEL - The integer Y values of all pixels within the region
;
;*RESTRICTIONS:
;   Input variables must have the same numbers of elements
;   The region is assumed to be "simple", that is, all vertices point *away*
;        from the center. For more complex regions, use FILLREGION
;        (or separate the region into simple subsections).
;   It is assumed that the endpoints are contiguous, i.e. that the command
;        plot,[xendpt,xendpt(0)],[yendpt,yendpt(0)]
;   with !psym=0 will define a simple region.
;   Consecutive endpoints must be different, or no solution is possible
;   and program will quit.
;
;*PROCEDURES CALLED:
;   FILLBOX
;
;*MODIFICATION HISTORY:
;   written 13 July 1991 by GAR
;-
pro fillsimple,bdx,bdy,xpixel,ypixel
;
; check the input vertices to make sure x & y have the same numbers of 
; elements, and that each has > 1 element.
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'FILLSIMPLE, bdx, bdy, XPIXEL, YPIXEL'
  retall
endif
;
nelx = n_elements(bdx)
if (nelx le 1) then begin
  print,' Number of elements of BDX must be > 1. Returning.'
  retall
endif
;
nely = n_elements(bdy)
if (nely le 1) then begin
  print,' Number of elements of BDY must be > 1. Returning.'
  retall
endif
;
if (nelx ne nely) then begin
  print,' BDX and BDY must have the same number of elements. Returning.)'
  retall
endif
;
; find the outside limits of the region, the number of vertices, and the 
; position of the center
;
ix1 = min(bdx)
ix2 = max(bdx)
iy1 = min(bdy)
iy2 = max(bdy)
;
nend = n_elements(bdx)
xcen = total(bdx)/float(nend)
ycen = total(bdy)/float(nend)
; 
; use fillbox to fill the xpixel and ypixel with the x & y positions
; of all the pixels in the box which contains the region
;
xendpt = [ix1,ix1,ix2,ix2]
yendpt = [iy2,iy1,iy1,iy2]
fillbox,xendpt,yendpt,xpixel,ypixel,xmin,xmax,ymin,ymax
; 
; define these quantities (fillbox defines them, but we will need them here
; as well)
;
nxmin = fix( xmin - (xmin lt 0) )      ;next lower integer
nxmax = fix( xmax + (xmax gt 0) )      ;next highest integer
nymin = fix( ymin - (ymin lt 0) )      ;next lower integer
nymax = fix( ymax + (ymax gt 0) )      ;next highest integer
nxpix = nxmax - nxmin + 1
nypix = nymax - nymin + 1
ntot = long(nxpix)*long(nypix)
;
; Join the endpoints so that the boundary can be closed.
;
x = bdx
y = bdy
 if ( (x(nend-1) ne x(0)) or (y(nend-1) ne y(0)) ) then begin
  x = [bdx,bdx(0)] 
  y = [bdy,bdy(0)] 
  nend = nend + 1
endif
;
;  solve for straight line joining endpoints, segment by segment
;
ind = bytarr(ntot) + 1b
for nn=0,nend-2 do begin      
  x1 = x(nn) 
  x2 = x(nn+1) 
  y1 = y(nn) 
  y2 = y(nn+1)
  delx = x2 - x1 
  dely = y2 - y1
  if (!debug gt 3) then $
     print,' x1,x2,y1,y2,delx,dely ',x1,x2,y1,y2,delx,dely
;
;  if delx and dely both = 0, then return with an error message
;
  if ( (delx eq 0) and (dely eq 0) ) then begin
    print,' Endpoints ',ii,' and ',ii+1,' are the same. Returning.'
    retall
  endif
;
;  figure out which side of the line the center (xcen,ycen) is on
;  then for each line in y, find the x pixels to that side of the line
;
  if (delx eq 0) then begin 
    side = abs(xcen - x1)/(xcen - x1)
    ybd = indgen(nypix) + nymin
;
    xlim = x1
    ict = 0
    if (!debug gt 3) then $
       print,' x1,xcen,side,xlim,ict,nymin,nypix '$
            ,x1,xcen,side,xlim,ict,nymin,nypix
    for ii=0,long(nypix)-1 do begin
      if (side gt 0) then i1 = nxmin else i1 = nint(xlim + 0.5) 
      if (side gt 0) then i2 = nint(xlim - 0.5) else i2 = nxmax
      if (!debug gt 3) then $
         print,' ii,i1,i2,nxmin,nxmax,ict,ict+i1-nxmin,i2-i1+1 '$
              ,ii,i1,i2,nxmin,nxmax,ict,ict+i1-nxmin,i2-i1+1
      ind(ict+i1-nxmin) = bytarr(i2-i1+1)
      ict = ict + long(nxpix)
    endfor
    if (ict lt ntot) then ind(ict) = bytarr(ntot-ict)
;
  endif else begin 
    slope = dely/delx
    ybd = indgen(nypix) + nymin
;
    if (slope ne 0) then begin
      xbd = (ybd - y1)/slope + x1
      xav = total(xbd)/float(nypix)
      side = abs(xcen - xav)/(xcen - xav)
      if (!debug gt 3) then $
        print,' slope,xav,xcen,side,nymin,nypix '$
             ,slope,xav,xcen,side,nymin,nypix
;
      ict = 0
      for ii=0,long(nypix)-1 do begin
        xlim = xbd(ii)
        if (!debug gt 3) then $
           print,' ii,side,xlim,nxmin,nxmax ',ii,side,xlim,nxmin,nxmax
        if ( (xlim ge nxmin) and (xlim le nxmax) ) then begin
          if (side gt 0) then i1 = nxmin else i1 = nint(xlim + 0.5)
          if (side gt 0) then i2 = nint(xlim - 0.5) else i2 = nxmax
        if (!debug gt 3) then $
           print,' ii,i1,i2,nxmin,nxmax,ict,ict+i1-nxmin,i2-i1+1 '$
                ,ii,i1,i2,nxmin,nxmax,ict,ict+i1-nxmin,i2-i1+1
          ind(ict+i1-nxmin) = bytarr(i2-i1+1)
        endif else if (ii eq (nypix-1)) then ind(ict) = bytarr(nxpix)
        ict = ict + long(nxpix)
      endfor
      if (ict lt ntot) then ind(ict) = bytarr(ntot-ict)
;
    endif else begin
      side = abs(ycen - y1)/(ycen - y1)
      i1 = long(nxpix)*fix(y1-(y1 lt 0)-nymin)
      if (!debug gt 3) then print,' i1,nxpix,y1,ycen,side,nymin,ntot '$
         ,i1,nxpix,y1,ycen,side,nymin,ntot
      if (i1 gt 0) then begin
        if (side gt 0) then ind(0) = bytarr(i1) else ind(i1) = bytarr(ntot-i1) 
      endif
    endelse
  endelse
;
endfor
;
; use ind to select just those pixels within the boundary
;
ind = where(ind eq 1)
xpixel = xpixel(ind)
ypixel = ypixel(ind)
;
return       
end         ;pro fillsimple
