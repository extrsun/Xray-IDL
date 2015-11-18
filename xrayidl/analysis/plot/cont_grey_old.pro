pro cont_grey, im, hdr, im_grey, nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2, $ 
        TYPE=type, PUTINFO=putinfo, XTITLE=xtitle,  $
      YTITLE=ytitle, NLEVELS = nlevels, MAX_VALUE=max_value, LEVELS=levels, $
      YMINOR = yminor,follow=follow,colors=colors,interp=interp $   
	,subtitle=subtitle,corner=corner,equi=equi,rimage=rimage $
	,c_line=c_line,greymin=greymin,greymax=greymax,f_color=f_color,greylog=greylog $
	,xbar=xbar,xtics = xtics,ytics = ytics,c_labels=c_labels $
	,spline=spline,c_colors=c_colors,pscolor=pscolor,noerase=noerase $
	,barshiftx=barshiftx,barshifty=barshifty,barfactor=barfactor $
	,ntoppix=ntoppix,nbotpix=nbotpix,noimage=noimage,ylaboff=ylaboff
;+
; NAME:
;    IMCONTOUR
; PURPOSE:
;    Contour plot labeled with astronomical coordinates.  The type
;    of coordinate display is controlled by the keyword TYPE
;    Set TYPE=0 (default) to measure distances from the center of the image
;    (IMCONTOUR will decide whether the plotting units will be in
;    arc seconds, arc minutes, or degrees depending on image size.)
;    Set TYPE=1 for standard RA and Dec labeling
; CALLING SEQUENCE:
;    IMCONTOUR, im, hdr, [im_grey,  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2, 
;      XTITLE = , YTITLE = , NLEVELS= 
;               MAX_VALUE=, LEVELS=, TITLE =, SUBTITLE =, FOLLOW = , XMINOR =,
;               YMINOR = ]
;
; INPUTS:
;    IM - 2-dimensional image array
;    HDR - FITS header associated with IM, string array, must include
;          astrometry keywords.   IMCONTOUR will also look for the
;          OBJECT and IMAGE keywords, and print these if found and the 
;          PUTINFO keyword is set.
;    IM_GREY - 2-dimensional image array, if given, will be used for the 
;              grey-scaled plot
; OPTIONAL PLOTTING KEYWORDS:
;	/nx1,/nx2,/ny1,/ny2  = The coordinates of the lower left and the upper
;                            right coners of the image
;    TYPE - the type of astronomical labeling to be displayed.   Either set
;           TYPE = 0 distance to center of the image is
;                    marked in units of Arc seconds, arc minutes, or degrees
;           TYPE = 1 (default) astronomical labeling with Right ascension and 
;                    declination.
;    PUTINFO - If set then IMCONTOUR will add information about the image
;           to the right of the contour plot.  Information includes image
;           name, object, image center, image center, contour levels, and
;           date plot was made
;    The other keywords XTITLE, YTITLE, NLEVELS, MAX_VALUE, LEVELS,
;           TITLE, SUBTITLE, FOLLOW, XMINOR, and YMINOR have the same 
;            meaning as in the CONTOUR procedure.  
; colors  - gray scale color index
; equi - the equinox of the coordinate system. Def = J2000
; corner - if set, the output gives the normlized coordinates of xmin, xmax,
;		ymin,ymax of the image
; greymin - the image value as the lower limit of the grey plot
; f_color - the color of the plot frame. the default=!p.color (e.g., 0)
; c_line = vector containing line styles for individual contours with 
; 0 solid line, 1 dash line etc. see contour.
; spline - a small value (e.g., 0.001) will give better looking contours
; c_colors - singal value or a vector containing the color index(es) used 
;	to draw contours
;	pscolor - a keyword if set indicates a color postscript file is to
;		be produced.
;	barshiftx, barshifty, barfactor - the x and y shifts (in normalized
;	coordinates) and multiple size factor of the color bar (default=1;
;	the same linear size as the figure). These three parameters are
;	used to locate the bar in the figure.
;	ntoppix - the number of top color indexes in the color table
;	used for other purposes (e.g., drawing  the frame of the figure)
;	If the original color table is expanded to more levels (because
;	of device differences), ntoppix may need to be increased accordingly.
; NOTES:
;    (1) The contour plot will have the same dimensional ratio as the input
;        image array
;    (2) To contour a subimage, use HEXTRACT before calling IMCONTOUR
; 
;
;	To get a contour colors different from that of the axis, the keyword
; overrun problem of c_colors that is responsible for 
; both axis and contours is solved by calling contour a second time. 
; For device 'ps' c_color=0 (black), =!d.n_colors (white), and
; for device 'x', it is the reverse. wqd, oct 24 1993.
; The coordinate ticks (except the top axis) do reflect the true coordinates
; at the edges of the image and they are calculated with coordinates
; there although the ticks are not bended to point to proper directions.
; The ticks with the top axis is same as those at the bottom axis and are
; therefore not right ones. But I have not yet found a way to redraw
; the top axis. WQD, Oact 22, 1993
; PROCEDURES USED:
;     GREY. CHKIMHD, EXTAST, GETROT, TICPOS, TICLABEL, TIC_ONE, TICS, XYAD
;     RA_GRID, DEC_GRID, ADSTRING
; REVISION HISTORY:
;   Extracted and expanded by wqd (Aug 1992) from IMCONTOUR by including GREY 
; added keywords, pscolor, for a color postscript file
; keywords: barshiftx, barshifty, barfactor are added 
; wqd, Jan 7, 1994
;-
;common colors,r_orig,g_orig,b_orig,r_curr,g_curr,b_curr
;On_error,2                                 ;Return to caller

if N_params() LT 2 then begin             ;Sufficient parameters?
      print,'CALLING SEQUENCE - cont_grey,im,hdr, [im_grey, '
      print,'nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,'
      print,'TYPE=type, PUTINFO=putinfo, XTITLE=xtitle,YTITLE=ytitle'
      print,',NLEVELS = nlevels,MAX_VALUE=max_value,LEVELS=levels,'
      print,'YMINOR = yminor,follow=follow,colors=colors,interp=interp'   
      print,',subtitle=subtitle,corner=corner,equi=equi,rimage=rimage'
      print,',c_line=c_line,greymin=greymin,greymax=greymax,f_color=f_color,greylog=greylog'
      print,',xbar=xbar,xtics = xtics,ytics = ytics,c_labels=c_labels'
      print,',spline=spline,c_colors=c_colors,pscolor=pscolor,noerase=noerase'
      print,',barshiftx=barshiftx,barshifty=barshifty,barfactor=barfactor'
      print,',ntoppix=ntoppix,nbotpix=nbotpix,noimage=noimage'
     return
endif
if n_elements(c_labels) eq 0 then c_labels=0
if n_elements(spline) eq 0 then spline=0.005
;chkimhd,im,hdr,xsize,ysize      ;Make sure header is appropiate to image
check_fits,im,hdr,dimen,/notype     ;Make sure header is appropiate to image
xsize=dimen(0)
ysize=dimen(1)
;
; Set defaults if keywords not set
;
im_max = max( im, MIN=im_min )             ;Image MAX and MIN values
if not keyword_set(LEVELS) then  $         ;Default is 6 equally spaced levels
    levels = im_min + (findgen(6)+1)*(float(im_max)-im_min)/7.

if not keyword_set(NLEVELS) then  $        ;Default is NLEVELS = 6
    nlevels = N_elements(levels)

if not keyword_set(MAX_VALUE) then max_value = im_max

if not keyword_set(FOLLOW) then follow = 0
	
if n_elements(TYPE) eq 0 then type=1

if not keyword_set(XMINOR) then $
       if !X.MINOR EQ 0 then xminor = 5 else xminor = !X.MINOR

if not keyword_set(YMINOR) then $
       if !Y.MINOR EQ 0 then yminor = 5 else yminor = !Y.MINOR
; 
if n_elements(nx1) eq 0 then nx1=nint(0.15*!d.x_vsize)/float(!d.x_vsize)
; ;chosen to be the default value
if n_elements(nx2) eq 0 then nx2=nint(0.98*!d.x_vsize)/float(!d.x_vsize)
  ;could be overruled if noaspect is not set
if n_elements(ny1) eq 0 then ny1=nint(0.10*!d.y_vsize)/float(!d.y_vsize)
if n_elements(ny2) eq 0 then ny2=nint(0.98*!d.y_vsize)/float(!d.y_vsize)
 ;could be overruled if noaspect is not set
; 
; First get the grey-scaled image
if N_params() lt 3 then im_grey=im
grey,im_grey, nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,interp=interp,rimage=rimage,greymin=greymin,greymax=greymax,colors=colors,greylog=greylog,xbar=xbar,pscolor=pscolor,noerase=noerase,corner=corner,barshiftx=barshiftx,barshifty=barshifty,barfactor=barfactor $
,ntoppix=ntoppix,nbotpix=nbotpix,noimage=noimage
;
extast,hdr,cd,crpix,crval,noparams       ;Extract astrometry from header
if noparams LT 0 then $                  ;Does astrometry exist?
      message,'FITS header does not contain astrometry'
;
cd = cd/!RADEG & crval = crval/!RADEG
; 
; Adjust plotting window so that contour plot will have same dimensional 
; ratio as the image
; 
xlength = !D.X_VSIZE &  ylength = !D.Y_VSIZE
ratio=ylength/float(xlength)
xsize = fix(xsize)  &   ysize = fix(ysize)
xsize1 = xsize-1 & ysize1 = ysize-1
xratio = xsize/float(ysize)
yratio = ysize/float(xsize)

;if ylength*xratio le xlength then begin
;    xmax = nx1 + (ny2-ny1)*ylength*xratio/xlength
;    pos = [nx1,ny1,xmax,ny2]
;endif else  begin
;       xmax = nx2*ratio
 ;      pos = [nx1,ny1,xmax,ny1+(nx2-nx1)*xlength*yratio/ylength*ratio]
;endelse
pos =[nx1,ny1,nx2,ny2]
      xmax = nx2 ;added arbitrary
;
;
if n_elements(xtics) eq 0 then xtics=8
if n_elements(ytics) eq 0 then ytics=8
if !X.TICKS GT 0 then xtics = abs(!X.TICKS) else xtics = xtics
if !Y.TICKS GT 0 then ytics = abs(!Y.TICKS) else ytics = ytics
;pixx = xsize/xtics                ;Number of X pixels between tic marks
;pixy = ysize/ytics                ;Number of Y pixels between tic marks
pixx = float(xsize)/xtics                ;Number of X pixels between tic marks
pixy = float(ysize)/ytics                ;Number of Y pixels between tic marks
;getrot,cd,rot,cdelt               ;Get the rotation and plate scale
getrot,hdr,rot,cdelt               ;Get the rotation and plate scale
;xmid = xsize1/2.   &   ymid  = ysize1/2.
;xmid=sxpar(hdr,'crpix1')-1 & ymid=sxpar(hdr,'crpix2')-1
;xyad,hdr,xmid,ymid,ra_cen,dec_cen         ;Get Ra and Dec of image center
ra_cen=crval(0)*!RADEG & dec_cen=crval(1)*!RADEG
ra_dec = adstring(ra_cen,dec_cen,1)       ;Make a nice string
;
; Determine tic positions and labels for the different type of contour plots
;
if type NE 0 then begin                  ;RA and Dec labeling

     xedge = [0,xsize1,0]          ;X pixel values of the four corners
     yedge = [0,0,ysize1]          ;Y pixel values of the four corners
crpix=crpix+0.5
     xy2ad,xedge,yedge,cd,crpix,crval,a,d    ;RA and Dec of the corners
; xyad uses the Fortran coordinate assuming the center of the first pixel at 
; (1,1) while the standard coordinate used in FITS uses (0.5,0.5).
     a = a*!RADEG   &  d = d*!RADEG
;     pixx = xsize/xtics                ;Number of X pixels between tic marks
;     pixy = ysize/ytics                ;Number of Y pixels between tic marks
     tics,a(0),a(1),xsize,pixx,raincr,/RA  ;Find an even increment for RA
     tics,d(0),d(2),ysize,pixy,decincr    ;Find an even increment for Dec
     tic_one,a(0),pixx,raincr,botmin,xtic1,/RA     ;Position of first RA tic
     tic_one,d(0),pixy,decincr,leftmin,ytic1       ;Position of first Dec tic
;     nx = fix((xsize1-xtic1-1)/pixx)                 ;Number of X tic marks
;     ny = fix((ysize1-ytic1-1)/pixy)                 ;Number of Y tic marks
     nx = fix((xsize1-xtic1)/pixx)                 ;Number of X tic marks
     ny = fix((ysize1-ytic1)/pixy)                 ;Number of Y tic marks
     ra_grid = (botmin + findgen(nx+1)*raincr/4.)/!RADEG
     dec_grid = (leftmin + findgen(ny+1)*decincr/60.)/!RADEG
     ticlabels, botmin, nx+1, raincr, xlab, /RA, DELTA=1
     ticlabels, leftmin, ny+1, decincr, ylab,DELTA=1
     xpos = cons_ra( ra_grid,0,cd,crpix,crval )
     ypos = cons_dec( dec_grid,0,cd,crpix,crval )
; So only the upper grids are wrong (with the tics that should be different
; from at the bottom
; It would be ideal if the IDL can draw different tick marks at the 
; upper boundaries. WQD
;     xpos = cons_ra( ra_grid,crpix(1)-1,cd,crpix,crval )
;     ypos = cons_dec( dec_grid,crpix(0)-1,cd,crpix,crval )
; the coordinate ticks are calculated relative to the center position

     if n_elements(equi) eq 0 then begin
	xunits = 'RIGHT ASCENSION (J2000)' 
	yunits = 'DECLINATION (J2000)'
     endif else begin
	xunits = 'RIGHT ASCENSION '+ strtrim(equi,2)
     	yunits = 'DECLINATION '+strtrim(equi,2)
     endelse

endif else begin                          ;Label with distance from center

     ticpos,xsize1*cdelt(0),xsize,pixx,incrx,xunits     
     numx = fix(xsize/(2.*pixx))  
     ticpos,ysize1*cdelt(0),ysize,pixy,incry,yunits
      numy = fix(ysize/(2.*pixy))
      nx = 2*numx & ny = 2*numy
      xpos = xmid + (findgen(nx+1)-numx)*pixx
      ypos = ymid + (findgen(ny+1)-numy)*pixy
      xlab = string(indgen(nx+1)*incrx - incrx*numx,'(I3)')
      ylab = string(indgen(ny+1)*incry - incry*numy,'(I3)')
endelse
if keyword_set(ylaboff) ne 0 then  ylab=' '

; Get default values of XTITLE, YTITLE, TITLE and SUBTITLE
;
if not keyword_set(PUTINFO) then putinfo = 0

if N_elements(xtitle) EQ 0 then xtitle=xunits
;  if !X.TITLE eq '' then xtitle = xunits else xtitle = !X.TITLE

if N_elements(ytitle) EQ 0 then ytitle = yunits
;  if !Y.TITLE eq '' then ytitle = yunits else ytitle = !Y.TITLE

if N_elements(title) EQ 0 then title = !P.TITLE

if (N_elements(subtitle) EQ 0) and (putinfo LT 1) then $
    subtitle = 'CENTER:  R.A. '+ strmid(ra_dec,1,13)+'  DEC ' + $
               strmid(ra_dec,13,13) $
    else if n_elements(subtitle) ne 0 then subtitle = subtitle else $
	subtitle = !P.SUBTITLE
;
;if !d.name eq 'PS' and keyword_set(pscolor) eq 0 then begin
	if n_elements(c_colors) eq 0 then c_colors=!p.color ;black
	if n_elements(f_color) eq 0 then f_color=!p.color
;endif
if n_elements(c_line) eq 0 then c_line=replicate(0,nlevels);,c_line=[0,0,0,1,1,1] ;
;if n_elements(f_color) eq 0 then f_color=!p.color
;if n_elements(c_colors) eq 0 then c_colors=replicate(!p.color,nlevels)

;color_new,or_orig,og_orig,ob_orig
dif=where(c_colors ne f_color,ndif)

if ndif ne 0 then begin ;c_colors are overrunned by f_color if device='ps'
			;Thus need to draw the countours first
	contour,im,/noerase, $
;         XTICKS = nx, YTICKS = ny, $
	POSITION=pos, XSTYLE=1, YSTYLE=1,$
         XTICKV = xpos, YTICKV = ypos, XTITLE=xtitle, YTITLE=ytitle, $
         XTICKNAME = xlab,YTICKNAME = ylab $
	, TITLE = title, $
;         XTICKV = xpos, YTICKV = ypos, XTITLE='', YTITLE='', $
;         XTICKNAME = '', YTICKNAME = '', TITLE = '', $
          MAX_VALUE=max_value,SUBTITLE = subtitle, XMINOR = xminor $
	,FOLLOW = follow,YMINOR = yminor, $
	 LEVELS = levels, NLEVELS = nlevels,c_line=c_line, $
	c_labels=c_labels,spline=spline,c_colors=c_colors
	c_levels=[1.e22] ;equivalent to no contour
endif else c_levels=levels

contour,im,/noerase, $
;         XTICKS = nx, YTICKS = ny, $
	POSITION=pos, XSTYLE=1, YSTYLE=1,$
         XTICKV = xpos, YTICKV = ypos, XTITLE=xtitle, YTITLE=ytitle, $
         XTICKNAME = xlab, YTICKNAME = ylab, TITLE = title, $
          MAX_VALUE=max_value,SUBTITLE = subtitle, XMINOR = xminor $
	,FOLLOW = follow,YMINOR = yminor, $
	 LEVELS = c_levels, NLEVELS = nlevels,color=f_color,c_line=c_line, $
	c_labels=c_labels,spline=spline,c_colors=c_colors
; 
;  Write info about the contour plot if desired
;
if putinfo GE 1 then begin
;	if !d.name eq 'PS' then xmax = xmax +0.01 else  $
   xmax = xmax +0.05
;   object = sxpar(hdr,'OBJECT')
;   if !ERR ne -1 then xyouts,xmax,0.95,object,/norm
;   name = sxpar(hdr,'IMAGE')
;   if !ERR ne -1 then xyouts,xmax,0.90,name,/norm
	yshift=0.2
;   xyouts,xmax,yshift+0.85,'CENTER:',/NORM
;   xyouts,xmax,yshift+0.80,'R.A. '+ strmid(ra_dec,1,13),/NORM
;   xyouts,xmax,yshift+0.75,'DEC '+  strmid(ra_dec,13,13),/NORM
   xyouts,xmax,yshift+0.70,'IMAGE SIZE',/norm,color=f_color
   xyouts,xmax,yshift+0.65,'X: ' + strtrim(xsize,2),/norm,color=f_color
   xyouts,xmax,yshift+0.60,'Y: ' + strtrim(ysize,2),/norm,color=f_color
;   if n_elements(greymin) ne 0 then minim=greymin else $
	minim=min(im_grey(where(im_grey ne 0.)))
   xyouts,xmax,yshift+0.55,'MIN: ' + strtrim(minim,2),/norm,color=f_color
   xyouts,xmax,yshift+0.50,'MAX: ' + strtrim(max(im_grey),2),/norm,color=f_color
   xyouts,xmax,yshift+0.45,strmid(!STIME,0,17),/norm,color=f_color
   xyouts,xmax,yshift+0.40,'CONTOUR LEVELS:',/norm,color=f_color
   for i = 0,(nlevels < 9)-1 do $
      xyouts,xmax,yshift+0.35-0.05*i,string(i,'(i2)') + ':'+string(levels(i)),/NORM,COLOR=F_COLOR 
endif
return                                          
end                                         
