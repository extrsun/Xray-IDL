;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   MAKEMOI
;
;*PURPOSE:
;  makes a bit map of the modified insrument map
;  uses solutions for MOI given for RP150011
;
;*CALLING SEQUENCE:
;   makemoi,moisim, (xcenter,ycenter,xpixels,ypixels,blocksize=blocksize)
;
;*INPUT:
;   XCENTER - optional parameter which gives the x center of the output map
;             in  MOI pixels (i.e., units of MOI map, blocked by a factor of
;             16).
;             if no value is given, then a default of 256 is assumed.
;   YCENTER - optional parameter which gives the y center of the output map
;             if no value is given, then a default of 256 is assumed.
;   XPIXELS - size of output map in x dimension, in output pixels.
;             if no value is given, a default of 512 is assumed.
;   YPIXELS - size of output map in y dimension, in output pixels.
;             if no value is given, a default of 512 is assumed.
;
;*KEYWORDS:
;   BLOCKSIZE - optional parameter specifying blocksize of resulting map
;               default of 16 gives a 512 by 512 map of the entire detector
;               to specify default blocking, blocksize=0 (or 16)
;
;*OUTPUT
;   RESULT - a simulated bit map of the detector, where all pixels blocked
;            by ribs & or wires are given a value of zero, & all unblocked
;            pixels a value of 1
;
;   NOTE: if blocksize=0 is specified, then it doesn't matter what values
;         are given for xcenter, ycenter, xpixels and ypixels: the output map 
;         will be centered on [256,256] and the entire MOI file will be 
;         included.
;
;*PROCEDURES CALLED
;   NINT.
;   MOI_LIMIT_PIX.
;   NINTREG.
;   MOI_BOX.
;   FILLBOX.
;   FILLSIMPLE.
;
;*MODIFICATION HISTORY:
;	written 5 July 1991 by GAR
;       9 July 1991 by GAR to allow only part of the MOI to be 
;         simulated, at higher resolution.
;-
pro makemoi,moisim,xcenter,ycenter,xpixels,ypixels,blocksize=blocksize
;
if (n_elements(blocksize) eq 0) then blocksize=0   ;default blocking
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' MAKEMOI, MOISIM, (xcenter, ycenter, xpixels, ypixels),'
  print,'          blocksize=blocksize (0)'
  return
endif
if (npar lt 5) then ypixels = 512
if (npar lt 4) then xpixels = 512
if (npar lt 3) then ycenter = 256
if (npar lt 2) then xcenter = 256
instr = 'RP'
;
; first, set up the blocking factor and the desired number of pixels
;
if (blocksize eq 0) then begin
  bfact = 1.0
  xcen = 256
  ycen = xcen
  xsize = 512
  ysize = xsize
;
  xlim1 = 0
  xlim2 = 511
  ylim1 = xlim1
  ylim2 = xlim2
endif else begin
  blocksz = nint(blocksize)
  bfact = 16./float(blocksz)    ;all solutions done for blocksz=16 & npix=512
;
  xcen = xcenter*bfact
  ycen = ycenter*bfact
  xsize = xpixels
  ysize = ypixels
;
  xlim1 = nint( xcen - xsize/2.) > 0
  xlim2 = nint( xcen + xsize/2.) < nint( 512*bfact ) - 1
  ylim1 = nint( ycen - xsize/2.) > 0
  ylim2 = nint( ycen + xsize/2.) < nint( 512*bfact ) - 1
;
  xcen = nint( (xlim2 + xlim1)/2. )      ;redefine center just in case
  ycen = nint( (ylim2 + ylim1)/2. )      ;limits were outside allowed range
  xsize = xlim2 - xlim1 + 1
  ysize = ylim2 - ylim1 + 1
endelse
boxslope = abs( float(ylim2 - ylim1)/float(xlim2 - xlim1) )
moisim = bytarr(xsize,ysize)
if (!debug gt 0) then print,' Center = ',xcen,ycen,' size =',xsize,ysize
;
;  these are the results of the solutions for the rib structure
;  derived from RP150011.MOI
;
radout = 87.5*bfact
radin  = 81.0*bfact
radmax = 225.*bfact
arib = [1.254372e-03,1.254372e-03,-1.606991e-03,-1.606991e-03, $
       1.00326,1.00326,-0.998118,-0.998118]
brib = [256.374,256.374,245.621,245.621,10.4727,10.4727, $
       501.501,501.501]*bfact
ribmin = [  22, 328,  29, 338,  85, 302,  85,  302]*bfact
ribmax = [ 162, 471, 174, 477, 187, 404, 187,  406]*bfact
delrib = [2, 2, 2, 2, 4, 4, 4, 4]*bfact
nribs = 8
x0 = 245.35*bfact       ;where the apparent center of the detector is
y0 = 256.65*bfact      
;
; do the ribs first: y = a*x + b, except for rib 2. For rib 2, x = a*y + b
;
if (!debug gt 0) then print,' Now calculating pixels for ribs'
;
for nn = 0,nribs-1 do begin
  del = nint( delrib(nn) )
  bdx = [-999]
  bdy = [-999]
;
;  get the x & y endpoints of the region that is filled by the rib
;  use skip to make sure that the rib is going to cover some pixels
;
  if ( (nn eq 2) or (nn eq 3) ) then begin    ;solve for bdx in terms of bdy
    iy1 = ribmin(nn) > ylim1
    iy2 = ribmax(nn) < ylim2
;
    np = iy2 - iy1 + 1
    if (np gt 0) then begin
      bdy = [bdy,findgen(np)+iy1]
      bdy = bdy(1:*)
      bdx = arib(nn)*bdy + brib(nn)
    endif
;
  endif else begin
    if ( abs(arib(nn)) le boxslope ) then begin     ;truncate at x edges
      ix1 = ribmin(nn) > xlim1
      ix2 = ribmax(nn) < xlim2
;
      np = ix2 - ix1 + 1
      if (np gt 0) then begin
        bdx = [bdx,findgen(np)+ix1]
        bdx = bdx(1:*)
        bdy = arib(nn)*bdx + brib(nn)
      endif 
    endif else begin                               ;truncate at y edges
      iy1 = arib(nn)*ribmin(nn) + brib(nn) > ylim1
      iy2 = arib(nn)*ribmax(nn) + brib(nn) < ylim2
;
      np = iy2 - iy1 + 1
      if (np gt 0) then begin
        bdy = [bdy,findgen(np)+iy1]
        bdy = bdy(1:*)
        bdx = ( bdy - brib(nn) )/arib(nn)
      endif 
    endelse
  endelse
  if (!debug gt 0) then print,nn,n_elements(bdx),n_elements(bdy)
;
  for ii = -del,del do begin
    fi = float(ii)
    ix = bdx
    iy = bdy + fi
    if ( (nn eq 2) or (nn eq 3) ) then begin
      ix = bdx + fi
      iy = bdy
    endif
;
    moi_limit_pix,ix,iy,xlim1,xlim2,ylim1,ylim2,x0,y0,radin,radmax,skip
    if (skip eq 0) then begin
      ix = ix - xcen + xsize/2
      iy = iy - ycen + ysize/2
      moisim(ix,iy) = 1
    endif
;
  endfor
;
endfor
;
; now do the circle for the inner region
; first fill in an annular box with the correct inner & outer edges
;
cfin = cos(45.*3.14159/180.)
ix1 = x0 - radout
ix2 = x0 - radin*cfin
ix3 = x0 + radin*cfin
ix4 = x0 + radout
iy1 = y0 - radout
iy2 = y0 - radin*cfin
iy3 = y0 + radin*cfin
iy4 = y0 + radout
if (bfact gt 1.0) then begin
  pow = nint( alog(bfact)/alog(2) )
  fact = exp(alog(2)*pow)
endif else fact = 1.0
delx = (ix4 - ix1)/fact
dely = (iy3 - iy2 - 2)/fact
;
;  fill in "sections" which will make up the annulus
;  check to make sure that the pixels in each section are within the
;  region of the output map, and also limit them by radius
;
if (!debug gt 0) then print,' Now calculating pixels for inner ring'
;
for nn=0,4*fact-1 do begin
  if (nn lt (fact*2)) then begin
    nbr = fact
    xbeg = (nn lt nbr)*(ix1 + nn*delx) + (nn ge nbr)*ix1
    xend = (nn lt nbr)*(xbeg + delx)   + (nn ge nbr)*ix2
    ybeg = (nn lt nbr)*iy1             + (nn ge nbr)*(iy2+1+(nn-nbr)*dely)
    yend = (nn lt nbr)*iy2             + (nn ge nbr)*(ybeg+dely)
  endif else begin
    nbr = fact*3
    xbeg = (nn lt nbr)*ix3 + (nn ge nbr)*(ix1+(nn-nbr)*delx)
    xend = (nn lt nbr)*ix4 + (nn ge nbr)*(xbeg+delx)
    ybeg = (nn lt nbr)*(iy2+1+(nn-nbr+fact)*dely) + (nn ge nbr)*iy3
    yend = (nn lt nbr)*(ybeg+dely) + (nn ge nbr)*iy4
  endelse
  moi_box,xlim1,xlim2,ylim1,ylim2,xbeg,xend,ybeg,yend,bdx,bdy,skip
  if (!debug gt 0) then print,nn,n_elements(bdx),n_elements(bdy)
;
  if (skip eq 0) then begin
    moi_limit_pix,bdx,bdy,xlim1,xlim2,ylim1,ylim2,x0,y0,radin,radout,skip
    if (skip eq 0) then begin
      bdx = bdx - xcen + xsize/2
      bdy = bdy - ycen + ysize/2
      moisim(bdx,bdy) = 1
    endif
  endif
  if (!debug gt 1) then stop,' Stopping in loop ',nn
endfor
;
;
; now do the coarse wire grid
;
del = bfact
awire = [32.7365,43.7119]*bfact
bwire = [11.4825,11.4891]*bfact
nwires = 38
xwire = awire(0) + findgen(nwires)*bwire(0)
ywire = awire(1) + findgen(nwires)*bwire(1)
;
;  first do the vertical wires, and then the horizontal wires
;
if (!debug gt 0) then print,' Now calculating pixels for coarse wires'
;
for nn = 0,1 do begin
  if (nn eq 0) then begin          ;set up for the vertical wires
    wirepos = xwire
    lim1 = xlim1
    lim2 = xlim2
    mapsize = ysize
  endif else begin                      ;set up for the horizontal wires
    wirepos = ywire
    lim1 = ylim1
    lim2 = ylim2
    mapsize = xsize
  endelse
  ntot = (2.*del+1.)*mapsize
;
  rad = where( (wirepos ge (lim1-del)) and (wirepos le (lim2+del)) )
  if (rad(0) ge 0) then begin
    wirepos = wirepos(rad)
    nwires = n_elements(wirepos)
;
    for ii=0,nwires-1 do begin
      bdx = fltarr(ntot)
      bdy = fltarr(ntot)
      ict = 0
      for jj=-del,del do begin
        fj = float(jj)
        if (nn eq 0) then begin
          bdx(ict) = fltarr(ysize)+wirepos(ii)+fj
          bdy(ict) = findgen(ysize) + ylim1
        endif else begin
          bdx(ict) = findgen(xsize) + xlim1
          bdy(ict) = fltarr(xsize)+wirepos(ii)+fj
        endelse
        ict = ict + mapsize 
      endfor
      if (!debug gt 0) then print,ii,n_elements(bdx),n_elements(bdy)
;
      moi_limit_pix,bdx,bdy,xlim1,xlim2,ylim1,ylim2,x0,y0,0.,radmax,skip
      if (skip eq 0) then begin
        bdx = bdx - xcen + xsize/2
        bdy = bdy - ycen + ysize/2
        moisim(bdx,bdy) = 1
      endif
;
    endfor
  endif
endfor
;
; now do the 8 wedges on the ribs (and the 8 "feet")
;
nwedge = 16
xwedge = fltarr(3,nwedge)
ywedge = fltarr(3,nwedge)
xwedge(0,0) = [178.50,190.25,178.50] & ywedge(0,0) = [201.25,189.25,189.25]
xwedge(0,1) = [237.75,251.25,244.96] & ywedge(0,1) = [169.50,169.25,159.63]
xwedge(0,2) = [298.50,313.50,310.18] & ywedge(0,2) = [189.25,203.25,189.25]
xwedge(0,3) = [158.25,159.50,149.63] & ywedge(0,3) = [264.75,245.75,257.08]
xwedge(0,4) = [189.50,177.75,177.75] & ywedge(0,4) = [322.75,310.50,322.75]
xwedge(0,5) = [332.00,331.50,338.87] & ywedge(0,5) = [263.50,247.50,256.17]
xwedge(0,6) = [300.50,312.00,312.00] & ywedge(0,6) = [323.50,309.75,323.50]
xwedge(0,7) = [236.25,253.25,245.33] & ywedge(0,7) = [342.75,342.75,352.65]
xwedge(0,8) = [ 79.75, 91.75, 91.75] & ywedge(0,8) = [102.75,102.75, 91.25]
xwedge(0,9) = [235.75,255.75,245.36] & ywedge(0,9) = [ 31.75, 30.25, 41.36]
xwedge(0,10) =[398.50,411.75,398.50] & ywedge(0,10) =[ 90.50,103.00,103.00]
xwedge(0,11) =[ 21.50, 21.50, 29.79] & ywedge(0,11) =[248.50,263.50,256.79]
xwedge(0,12) =[470.50,470.25,462.10] & ywedge(0,12) =[266.25,247.25,255.75]
xwedge(0,13) =[ 83.25, 91.50, 91.50] & ywedge(0,13) =[409.50,418.00,409.50]
xwedge(0,14) =[237.25,255.75,244.64] & ywedge(0,14) =[479.50,480.25,474.33]
xwedge(0,15) =[398.75,409.00,398.75] & ywedge(0,15) =[419.00,410.25,410.25]
xwedge = xwedge*bfact
ywedge = ywedge*bfact
;
if (!debug gt 0) then print,' Now calculating pixels for wedges and feet'
;
for nn=0,nwedge-1 do begin
  bdx = xwedge(*,nn)
  bdy = ywedge(*,nn)
  skip = 0
  if ( (max(bdx) lt xlim1) or (min(bdx) gt xlim2) ) then skip = 1
  if ( (max(bdy) lt ylim1) or (min(bdy) gt ylim2) ) then skip = 1
  if (skip eq 0) then begin
    if (!debug gt 0) then print,nn,max(bdx)-min(bdx),max(bdy)-min(bdy)
    fillsimple,bdx,bdy,ix,iy
    moi_limit_pix,ix,iy,xlim1,xlim2,ylim1,ylim2,x0,y0,radin,radmax,skip2
    if (skip2 eq 0) then begin
      ix = ix - xcen + xsize/2
      iy = iy - ycen + ysize/2
      moisim(ix,iy) = 1
    endif
  endif
endfor
;
moisim = 1b - moisim              ;reverse the logic
;
return
end             ;pro makemoi
