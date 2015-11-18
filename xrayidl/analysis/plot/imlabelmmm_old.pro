pro imlabelmmm,hdr,pos=pos,xtitle=xtitle,ytitle=ytitle,chsize=chsize, $
  c_colors=c_colors,f_color=f_color,ntickx=ntickx,nticky=nticky, $
  xunitlv=xunitlv,yunitlv=yunitlv,toplabel=toplabel,botlabel=botlabel,leftlabel=leftlabel
;+
; NAME:
;       IMLABELMMM
;
; PURPOSE:
;       To label a cont_grey image with right ascension and declination
;	grid lines using the astrometry info present in the header.  Unlike
;	imlabel.pro upon which this has a remote resemblance to, IMLABELMMM
;	will plot astrometrically-correct grids that include a celestial pole.
;  	Many errors in imlabel.pro have been corrected
;	Set !grid = 1 to make a grid, and !grid = 0 for regular tick marks.  
;* SEQUENCE:
;	IMLABELMMM, hdr, pos =, [ XTITLE =, YTITLE =, CHSIZE =, C_COLORS =,
;	  F_COLOR =, NTICKX =, NTICKY =, XUNITLV =, YUNITLV =, POLE =,
;	  TOPLABEL =, BOTLABEL = ]
;
; INPUTS:
;	hdr      - FITS image header array;
;	pos	 - coordinates of lower left and upper right corners of plot
;		   in normalized coordinates;
;	c_colors - contour color;
;	f_color	 - plot color;
;
; OPTIONAL INPUTS:
;	xtitle	 - x-axis annotation;
;	ytitle	 - y-axis annotation;
;	chsize - scale factor for size of character labels (def = 1.0)
;	ntickx/y - number of x- or y-ticks; setting one of these parameters
;		   but not the other will result the tick spacing in the
;		   other axis to be the same; default is a tick spacing that
;		   gives the x-axis 10 tick marks.
;	xunitlv	 - how far below the x-axis to place the x-axis annotation
;		   in normalized units of the space from the x-axis to the
;		   edge of the plot device; default is .00;
;	yunitlv	 - how far to the left of the y-axis to place the y-axis
;		   annotation in normalized units of the space from the
;		   y-axis to the edge of the plot device; default is .4;
;	toplabel - default = 0, meaning RA labels are not plotted along top
;		   axis; set = 1 to turn on top axis label plotting;
;	botlabel - default = 1, meaning RA labels are plotted along bottom
;		   axis; set = 0 to turn off bottom axis label plotting.
;
; OUTPUTS:
;	None
;
; SYSTEM VARIABLES: 
;	The system variable !GRID should be set to 1 to display 
;	full grid lines, or set to 0 to see just tic marks.
;
; RESTRICTIONS:
;	The rotation angle must be 0 (north is up) in the image.
;	IMLABELMMM has not been tested for REALLY large areas, such
;	as those where constant RA grid lines start to wrap around.
;
;*Subroutines called:
; bracket_v.pro.
;
; REVISION HISTORY:
;	Written by kachun 13 July, 1994; program is similar in some ways
;	to the original imlabel except that this has corrected astrometry,
;	and the program can create grids at high latitudes.
;-

;** Check for required param

 npar = N_params()
 if npar eq 0 then begin
   print,'Syntax - IMLABELMMM, hdr, pos =, [ XTITLE =, YTITLE =, CHSIZE = ]'
   print,'   C_COLORS =, F_COLOR =, NTICKX =, NTICKY =, XUNITLV =, YUNITLV ='
   return
 endif


if n_elements(leftlabel) eq 0 then leftlabel = 1
if n_elements(toplabel) eq 0 then toplabel = 0
if n_elements(botlabel) eq 0 then botlabel = 1
if n_elements(xunitlv) eq 0 then xunitlv = .2
if n_elements(yunitlv) eq 0 then yunitlv = .3
if n_elements(c_colors) eq 0 then c_colors = 241 ;Assume white.

nx1 = pos(0) & ny1 = pos(1)
nx2 = pos(2) & ny2 = pos(3)

;** Define constants to renormalize plots:

dx = nx2 - nx1 & dy = ny2 - ny1

;** Check to see if IMLABELMMM has header hdr with given parameters (1,7,1):

 zparcheck,'IMLABELMMM',hdr,1,7,1,'FITS header array'

;** Define some variables:

  if n_elements(chsize) eq 0 then chsize = !p.charsize
  plint = 500 ;; (Arbitrary size of vectors to plot when calculating lines
              ;;  of constant RA and dec.)

;** Get size of image in pixels from header:

xsize = sxpar(hdr,'NAXIS1')
ysize = sxpar(hdr,'NAXIS2')

;extast, hdr, para,  noparams ;new astro library
;cd=para.cd
;crpix=para.crpix
;crval=para.crval
crpix=sxpar(hdr,'crpix*')
crval=sxpar(hdr,'crval*')
extast, hdr, cd,  crpix, crval, noparams
;if noparams lt 0 then $               ;** Does astrometry exist?
;  message,'FITS header does not contain astrometry'

;** Redefine astrometry into display pixels (do everything in normalized
;** coordinates):
;crval = crval/!radeg & cd = cd/!radeg

;** Check on center RA and DEC coordinates [note that xy2ad input crpix
;** is in SDAS FORTRAN format with array beginning at (1,1) and inputs
;** x and y are in IDL format with array beginning at (0,0)]:

;  xy2ad, crpix(0)-1., crpix(1)-1., cd, crpix, crval, a, d

;  xy2ad, crpix(0)-1., crpix(1)-1., para,  a, d
;stop
;trans_radian,crpix(0),crpix(1),crval(0),crval(1),a,d,/deg
;  a = a*!radeg
;  d = d*!radeg
a=crval(0)
d=crval(1)
  if abs(d) eq d then declin = 'pos' else declin = 'neg'
  radec, a, d, i1, i2, i3, i4, i5, i6
  acen = string(i1,format='(I2)') + ':' + string(i2,format='(I2)') + ':' + $
         string(i3,format='(F5.1)')
  dcen = string(i4,format='(I3)') + ':' + string(i5,format='(I2)') + ':' + $
         string(fix(i6),format='(I2)')
  cen = 'CENTER PIXEL:  R A: ' + acen + '  DEC: ' + dcen
  print, cen

;** Get pixel size in arcseconds:

  pixsize = abs(sxpar(hdr,'CDELT1'))
  pixsize = pixsize * 3600.
;  crval = crval*!radeg

;** Check to see if a celestial pole is within the image boundaries (currently
;** the southern celestial pole will be plotted around if both poles are
;** present in the image):

  n_pole = 0 & s_pole = 0
  trans_dist, a, d, 0., +90., pxval, pyval, pixsize = pixsize, /deg
  trans_dist, a, d, 0., -90., nxval, nyval, pixsize = pixsize, /deg
  if abs(pxval) lt xsize*.5 and abs(pyval) lt ysize*.5 then n_pole = 1 else $
    if abs(nxval) lt xsize*.5 and abs(nyval) lt ysize*.5 then s_pole = -1 else $
    pole = 0
  if n_pole eq 1 and s_pole eq 0 then begin
    pole = n_pole
    print,'North celestial pole is in image.'
  endif
  if s_pole eq -1 and n_pole eq 0 then begin
    pole = s_pole
    print,'South celestial pole is in image.'
  endif
  if n_pole eq 1 and s_pole eq -1 then begin
    print,'WARNING:  both celestial poles are in image; program not yet able to
    print,'          handle this.'
  endif

;** Find range of RA and DEC that we'll have to worry about; range in
;** RA will be range of RA along top axis [for (+) declinations] or along
;** bottom axis [for (-) declinations]; range in DEC will be minmax of DEC
;** along vertical line running through reference pixel.  (Assumes that
;** reference pixel is at the CENTER of the image.)

;** Find minimum and maximum DEC along line at constant
;** RA intersecting reference pixel:

  trans_loct, 0, -crpix(1), crval(0), crval(1), ra0, dec0, /deg, pixsize=pixsize
  trans_loct, 0,  ysize-crpix(1), crval(0), crval(1), ra0, dec1, /deg, pixsize=pixsize

;** Find RA and DEC at the lower left, upper left, and lower right corners
;** of the image:

  trans_loct, -crpix(0), -crpix(1), crval(0), crval(1), botmin, leftmin, /deg, $
    pixsize = pixsize
  trans_loct,  xsize-crpix(0), -crpix(1), crval(0), crval(1), botmax, rightmin, /deg,$
    pixsize = pixsize
  trans_loct, -crpix(0),  ysize-crpix(1), crval(0), crval(1), topmin, leftmax, /deg, $
    pixsize = pixsize

  ;** Find range of RA and DEC if no pole is in the image:
  if pole eq 0 then begin

    minra = min([botmin,botmax,topmin,ra0])
    maxra = max([botmin,botmax,topmin,ra0])
    rasize = maxra - minra

    mindec = min([leftmin,leftmax,rightmin,dec0,dec1])
    maxdec = max([leftmin,leftmax,rightmin,dec0,dec1])
    decsize = maxdec - mindec

  ;** Find range of RA and DEC if a pole is in the image:
  endif else begin

    if pole eq 1 then begin
      maxdec = 90.
      mindec = min([leftmin,leftmax,rightmin,dec0,dec1])
    endif
    if pole eq -1 then begin
      mindec = -90.
      maxdec = max([leftmin,leftmax,rightmin,dec0,dec1])
    endif
    decsize = maxdec - mindec

    maxra = 360.
    minra = 0.
    rasize = maxra - minra

  endelse

  ;** Define vector (of arbitrary size plint = 100) of RA and DEC values:

  ravec = minra + findgen(plint+1)*rasize/plint
  decvec = mindec + findgen(plint+1)*decsize/plint

;** Find approximate pixel interval between tick marks; use inputs ntickx
;** and nticky for number of ticks if present:

  if n_elements(ntickx) eq 0 and n_elements(nticky) eq 0 then begin
    pixx = fix(xsize/10)
    pixy = fix(xsize/10)
  endif else if n_elements(ntickx) ne 0 and n_elements(nticky) eq 0 then begin
    pixx = fix(xsize/ntickx)
    pixy = fix(xsize/ntickx)
  endif else if n_elements(ntickx) eq 0 and n_elements(nticky) ne 0 then begin
    pixx = fix(ysize/nticky)
    pixy = fix(ysize/nticky)
  endif else begin
    pixx = fix(xsize/ntickx)
    pixy = fix(ysize/nticky)
  endelse

;** Determine tic size and label units:

   tics, min([botmin,botmax]), max([botmin,botmax]), xsize, pixx, raincr, /RA
   tics, min([leftmin,leftmax]), max([leftmin,leftmax]), ysize, pixy, decincr
   numtica = rasize / (raincr*360./24./60.) + 1
   numticd = decsize / (decincr/60.) + 1

;** Determine pos and value at 1st tic

   tic_one, minra, pixx, raincr, botmin2, xtic1, /RA
   tic_one, mindec, pixy, decincr, leftmin2, ytic1

;** Create tic labels; first define sign of RA and DEC increments:

;   radir = abs(botmax-botmin)/abs(botmax-botmin)
;   decdir = abs(leftmax-leftmin)/abs(leftmax-leftmin)
;   ticlabels, botmin2, numtica, radir*raincr, ticlabx, /RA
;   ticlabels, leftmin2, numticd, decdir*decincr, ticlaby
   ticlabels, botmin2, numtica, raincr, ticlabx, /RA
   ticlabels, leftmin2, numticd, decincr, ticlaby

   if n_elements(xtitle) ne 0 then xunits = xtitle else xtitle = ''
   if n_elements(ytitle) ne 0 then yunits = ytitle else xtitle = ''

;** Label tic marks in ra and dec

   ;** Get and plot constant DEC lines (remember that decincr is in arcmin;
   ;** program assumes DEC increase upwards (north) in plot):

   n = 0
   ytold = 0.
   for m=0,numticd-1 do begin

     ;** Create vector consisting of constant DEC values:
;     constdec = replicate(leftmin2+decdir*decincr*fix(m)/60., plint+1)
     constdec = replicate(leftmin2+decincr*fix(m)/60., plint+1)
     ;** Calculate x and y pixel positions of the grid line with constant DEC:
     trans_dist, a, d, ravec, constdec, xval, yval, pixsize=pixsize, /deg
     xval = xval + crpix(0) - 1 & yval = yval + crpix(1) - 1
     xdis = nx1 + dx*xval/(xsize-1) & ydis = ny1+dy*yval/(ysize-1)
     if !grid eq 1 then plots, xdis, ydis, /normal, color=c_colors, noclip=0

     ;** Find normalized coordinate values of the tick mark:

     if min(xdis) lt nx1 then begin

       ;** Find bins closest to the edge:
       bracket_v, xdis, nx1, i_values=i_values, m_values=m_values
       if total(i_values) ne -1 then niv = n_elements(i_values(*,0)) else $
         niv = 0
       if total(m_values) ne -1 then nmv = n_elements(m_values) else nmv = 0

       yt1_vec = fltarr(niv+nmv)

       ;** Interpolate between the bins closest to edge to find out
       ;** where to place the tick mark:
       if niv ne 0 then $
         for l=0,niv-1 do begin
           linterp, xdis(i_values(l,0):i_values(l,1)), $
             ydis(i_values(l,0):i_values(l,1)), nx1, yt1
           yt1_vec(l) = yt1
         endfor
       if nmv ne 0 then $
         for l=0,nmv-1 do begin
           linterp, xdis(m_values(l,0):m_values(l,1)), $
             ydis(m_values(l,0):m_values(l,1)), nx1, yt1
           yt1_vec(niv+l) = yt1
         endfor

       ysel = where(yt1_vec ge ny1 and yt1_vec le ny2,nysel)

       if nysel ne 0 then begin

         yt1_vec = yt1_vec(where(yt1_vec ge ny1 and yt1_vec le ny2))
         yt1 = min(yt1_vec(where(yt1_vec gt ytold)))
         ytold = yt1

         ;** Label tick marks (every other tick):
	if leftlabel ne  0 then begin
         if total( where(ydis gt ny1) ) gt 0 and (n eq 0) and $
           (m/2. eq float(m/2)) then begin
             label = strtrim(ticlaby(m/2.),2)
             xyouts, nx1*.95, yt1, label, /normal, size=chsize*0.9, $
               color=f_color, alignment=1.0
             n = 1
         endif else n = 0
 	endif

       endif

     endif else nysel = 0

     ;** Plot tick marks if no grid is plotted:

     if !grid eq 0 and nysel ne 0 then $
       plots, [nx1,nx1+dx/50.], [yt1,yt1], /normal, color=f_color

   endfor

   ;** Get and plot constant RA lines (remember that raincr is in minutes;
   ;** program assumes RA increases towards the left (east) in plot):

   n = 0
   for m=0,numtica-1 do begin

     ;** Create vector consisting of constant RA values:
;     constra = replicate(botmin2+radir*raincr*fix(m)*360./24./60., plint+1)
     constra = replicate(botmin2+raincr*fix(m)*360./24./60., plint+1)
     ;** Calculate x and y pixel positions of the grid line with constant RA:
     trans_dist, a, d, constra, decvec, xval, yval, pixsize=pixsize, /deg
     xval = xval + crpix(0) - 1 & yval = yval + crpix(1) - 1
     xdis = nx1 + dx*xval/(xsize-1) & ydis = ny1 + dy*yval/(ysize-1)
     if !grid eq 1 then plots, xdis, ydis, /normal, color=c_colors, noclip=0

     ;** Find normalized coordinate values of the tick mark:

     ;** Plot RA labels along bottom axis if parameter is set:
     if botlabel eq 1 then begin

       if min(ydis) lt ny1 then begin

         ;** Find bins closest to bottom edge:
         bracket_v, ydis, ny1, i_values=i_values, m_values=m_values
         if total(i_values) ne -1 then niv = n_elements(i_values(*,0)) else $
           niv = 0
         if total(m_values) ne -1 then nmv = n_elements(m_values) else nmv = 0
 
         xt1_vec = fltarr(niv+nmv)
 
         ;** Interpolate between the bins closest to edge to find out
         ;** where to place the tick mark:
         if niv ne 0 then $
           for l=0,niv-1 do begin
             linterp, ydis(i_values(l,0):i_values(l,1)), $
               xdis(i_values(l,0):i_values(l,1)), ny1, xt1
             xt1_vec(l) = xt1
           endfor
         if nmv ne 0 then $
           for l=0,nmv-1 do begin
             linterp, ydis(m_values(l,0):m_values(l,1)), $
               xdis(m_values(l,0):m_values(l,1)), ny1, xt1
             xt1_vec(niv+l) = xt1
           endfor

         xsel = where(xt1_vec ge nx1 and xt1_vec le nx2,nxsel)

         if nxsel ne 0 then begin

           xt1_vec = xt1_vec(xsel)
           xt1 = xt1_vec

           ;** Label tick marks (every other tick):

           if total( where(xdis gt nx1) ) gt 0 and (n eq 0) and $
             (m/2. eq float(m/2)) then begin
               label = strtrim(ticlabx(m/2),2)
               xyouts, xt1, ny1*.60, label, /normal, size=chsize*0.9, $
                 color=f_color, alignment=0.5
               n = 1
           endif else n = 0

         endif

       endif else nxsel = 0

       ;** Plot tick marks if no grid is plotted:

       if !grid eq 0 and nxsel ne 0 then $
         plots, [xt1,xt1], [ny1,ny1+dx/50.], /normal, color=f_color

     endif

     ;** Plot RA labels along top axis if parameter is set:
     if toplabel eq 1 then begin

       if max(ydis) gt ny2 then begin

         ;** Find bins closest to top edge:
         bracket_v, ydis, ny2, i_values=i_values, m_values=m_values
         if total(i_values) ne -1 then niv = n_elements(i_values(*,0)) else $
           niv = 0
         if total(m_values) ne -1 then nmv = n_elements(m_values) else nmv = 0
 
         xt1_vec = fltarr(niv+nmv)
 
         ;** Interpolate between the bins closest to edge to find out
         ;** where to place the tick mark:
         if niv ne 0 then $
           for l=0,niv-1 do begin
             linterp, ydis(i_values(l,0):i_values(l,1)), $
               xdis(i_values(l,0):i_values(l,1)), ny2, xt1
             xt1_vec(l) = xt1
           endfor
         if nmv ne 0 then $
           for l=0,nmv-1 do begin
             linterp, ydis(m_values(l,0):m_values(l,1)), $
               xdis(m_values(l,0):m_values(l,1)), ny2, xt1
             xt1_vec(niv+l) = xt1
           endfor
         xt1_vec = xt1_vec(where(xt1_vec ge nx1 and xt1_vec le nx2))
         xt1 = xt1_vec

         xsel = where(xt1_vec ge nx1 and xt1_vec le nx2,nxsel)

         if nxsel ne 0 then begin

           ;** Label tick marks (every other tick):
 
           if total( where(xdis gt nx1) ) gt 0 and (n eq 0) and $
             (m/2. eq float(m/2)) then begin
               label = strtrim(ticlabx(m/2),2)
               xyouts, xt1, ny2*1.01, label, /normal, size=chsize*0.9, $
                 color=f_color, alignment=0.5
               n = 1
           endif else n = 0

         endif
 
       endif else nxsel = 0
 
     ;** Plot tick marks if no grid is plotted:

     if !grid eq 0 and nxsel ne 0 then $
       plots, [xt1,xt1], [ny2,ny2-dx/50.], /normal, color=f_color
 
    endif

   endfor

 ;** Place x label:

 if botlabel eq 1 then $
   xyouts, nx1+.5*dx, xunitlv*ny1, xunits, size=chsize*1.00, $
     color=f_color, alignment=0.5, /normal
 if botlabel eq 0 and toplabel eq 1 then $
   xyouts, nx1+.5*dx, ny2*1.05, xunits, size=chsize*1.00, $
     color=f_color, alignment=0.5, /normal

 ;** Place y label:

if leftlabel ne 0 then $
 xyouts, yunitlv*nx1, ny1+.5*dy, yunits, size=chsize*1.00, $
    orientation=90.0, color=f_color, alignment=0.5, /normal

 ;** Plot box:

 plots, [nx1,nx1], [ny1,ny2], /normal, color=f_color
 plots, [nx1,nx2], [ny1,ny1], /normal, color=f_color
 plots, [nx2,nx2], [ny1,ny2], /normal, color=f_color
 plots, [nx1,nx2], [ny2,ny2], /normal, color=f_color

;stop,'End of imlabelmmm.pro.'
 return
 end


