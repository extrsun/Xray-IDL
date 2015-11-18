pro cont_grey, im, hdr, im_grey, nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2, $
  TYPE=type, PUTINFO=putinfo, XTITLE=xtitle,  $
  YTITLE=ytitle, NLEVELS = nlevels, MAX_VALUE=max_value, LEVELS=levels, $
  YMINOR = yminor,follow=follow,colors=colors,interp=interp, $
  subtitle=subtitle,corner=corner,equi=equi,rimage=rimage, $
  c_line=c_line,greymin=greymin,greymax=greymax,f_color=f_color, $
  greylog=greylog,xbar=xbar,c_labels=c_labels, $
  spline=spline,c_colors=c_colors,pscolor=pscolor,noerase=noerase, $
  barshiftx=barshiftx,barshifty=barshifty,barfactor=barfactor, $
  ntoppix=ntoppix,nbotpix=nbotpix,noimage=noimage, $
  ntickx=ntickx,nticky=nticky,botlabel=botlabel,toplabel=toplabel, $
  pthick=pthick,pcs=pcs,xunitlv=xunitlv,yunitlv=yunitlv,xposfac=xposfac $
 ,yposfac=yposfac,leftlabel=leftlabel,fullw=fullw,tick2=tick2,b_c=b_c $
	,TITLE=title,ticklen=ticklen,pos=pos,threshold=threshold
;+
; NAME:
;    CONT_GREY
; PURPOSE:
;    Contour plot labeled with astronomical coordinates.  The type of
;    coordinate display is controlled by the keyword TYPE.  Set TYPE=0
;    (default) to measure distances from the center of the image.
;    (CONT_GREY will decide whether the plotting units will be in
;    arc seconds, arc minutes, or degrees depending on image size.)
;    Set TYPE=1 for standard RA and Dec labeling.
; CALLING SEQUENCE:
;    CONT_GREY, im, hdr, [im_grey,  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,
;      XTITLE  = , YTITLE = , NLEVELS=
;      MAX_VALUE=, LEVELS=, TITLE =, SUBTITLE =, FOLLOW = , XMINOR =,
;      YMINOR = ]
; INPUTS:
;    IM      = 2-dimensional image array.
;    HDR     = FITS header associated with IM, string array, must include
;              astrometry keywords.   CONT_GREY will also look for the
;              OBJECT and IMAGE keywords, and print these if found and the
;              PUTINFO keyword is set.
;    IM_GREY = 2-dimensional image array, if given, will be used for the
;              grey-scaled plot.
; OPTIONAL PLOTTING KEYWORDS:
;    /nx1,/nx2,/ny1,/ny2
;            = The coordinates of the lower left and the upper right coners
;              of the image.
;    TYPE = the type of astronomical labeling to be displayed.   Either set
;           TYPE = 0 distance to center of the image is marked in units of
;                    arcseconds, arcminutes, or degrees;
;           TYPE = 1 (default) astronomical labeling with Right ascension and
;                    declination.
;    PUTINFO = If set then CONT_GREY will add information about the image
;              to the right of the contour plot.  Information includes image
;              name, object, image center, image center, contour levels, and
;              date plot was made.
;The other keywords XTITLE, YTITLE, NLEVELS, MAX_VALUE, LEVELS,
;  TITLE, SUBTITLE, FOLLOW, XMINOR, and YMINOR have the same
;  meaning as in the CONTOUR procedure.
; colors   = gray scale color index;
; equi     = the equinox of the coordinate system. Def = J2000;
; corner   = if set, the output gives the normlized coordinates of
;            xmin, xmax, ymin, ymax of the image;
; greymin  = the image value as the lower limit of the grey plot;
; f_color  = the color of the plot frame. the default=!p.color (e.g., 0);
; c_line   = vector containing line styles for individual contours with
;            0 solid line, 1 dash line etc. see contour;
; spline   = a small value (e.g., 0.001) will give better looking contours
; c_colors = single value or a vector containing the color index(es) used
;            to draw contours;
; pscolor  = a keyword if set indicates a color postscript file is to
;            be produced;
; barshiftx, barshifty, barfactor
;          = the x and y shifts (in normalized coordinates) and multiple
;            size factor of the color bar (default=1; the same linear size
;            as the figure). These three parameters are used to locate the
;            bar in the figure;
; ntoppix  = the number of top color indexes in the color table used for
;            other purposes (e.g., drawing  the frame of the figure).  If
;            the original color table is expanded to more levels (because
;            of device differences), ntoppix may need to be increased
;            accordingly.
;	xunitlv	 - how far above the bottum of the page to
;	           place the Y-axis annotation in normalized units of the 
;			distance to the Y-axis; default is .2;
;	yunitlv	 - how far from the edge of the plot device to place the Y-axis
;		   annotation in normalized units of the distance to the
;		   Y-axis of the PLOT; default is .3;
;		  for example, to place the lable to near the left edge of
;		the page, use yunitlv=0.1
; b_c - the background color code.
; NOTES:
;    (1) The contour plot will have the same dimensional ratio as the input
;        image array.
;    (2) To contour a subimage, use HEXTRACT before calling CONT_GREY.
;
; To get a contour colors different from that of the axis, the keyword
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
;
; PROCEDURES USED:
;     GREY. CHKIMHD, EXTAST, GETROT, TICPOS, TICLABEL, TIC_ONE, TICS, XYAD
;     RA_GRID, DEC_GRID, ADSTRING, IMLABEL_MMM, TRANS_DIST,TRANS_LOCT
; REVISION HISTORY:
;   Extracted and expanded by wqd (Aug 1992) by including GREY
; added keywords, pscolor, for a color postscript file
; keywords: barshiftx, barshifty, barfactor are added
; wqd, Jan 7, 1994
;
;-

if N_params() lt 2 then begin             ;Sufficient parameters?
  print,'CALLING SEQUENCE - cont_grey,im,hdr, [im_grey, '
  print,'nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,'
  print,'TYPE=type, PUTINFO=putinfo, XTITLE=xtitle,YTITLE=ytitle'
  print,',NLEVELS = nlevels,MAX_VALUE=max_value,LEVELS=levels,'
  print,'YMINOR = yminor,follow=follow,colors=colors,interp=interp'
  print,',subtitle=subtitle,corner=corner,equi=equi,rimage=rimage'
  print,',c_line=c_line,greymin=greymin,greymax=greymax,f_color=f_color,greylog=greylog'
  print,',xbar=xbar,c_labels=c_labels'
  print,',spline=spline,c_colors=c_colors,pscolor=pscolor,noerase=noerase'
  print,',barshiftx=barshiftx,barshifty=barshifty,barfactor=barfactor'
  print,',ntoppix=ntoppix,nbotpix=nbotpix,noimage=noimage'
 print,',pthick=pthick,pcs=pcs,xunitlv=xunitlv,yunitlv=yunitlv'
 print,',xposfac=xposfac,yposfac=yposfac,botlabel=botlabel,toplabel=toplabel'
print,',,fullw=fullw,tick2=tick2,b_c=b_c,pos=pos'
 return
endif
if n_elements(c_labels) eq 0 then c_labels=0
if n_elements(spline) eq 0 then spline=0.005
check_fits,im,hdr,dimen,/notype     ;Make sure header is appropiate to image
xsize=dimen(0)
ysize=dimen(1)
if n_elements(pthick) ne 0 then begin
	pthicko=!p.thick
	!p.thick=pthick
endif
if n_elements(pcs) ne 0 then begin
	pcso=!p.charsize
	!p.charsize=pcs
endif

if keyword_set(noerase) eq 0 then noerase=0
;**
;** Set defaults if keywords not set:
;**
im_max = max( im, MIN=im_min )         ;** Image MAX and MIN values;
if not keyword_set(LEVELS) then  $     ;** Default is 6 equally spaced levels;
   levels = im_min + (findgen(6)+1)*(float(im_max)-im_min)/7.
if not keyword_set(NLEVELS) then  $        ;Default is NLEVELS = 6
   nlevels = n_elements(levels)
if not keyword_set(MAX_VALUE) then max_value = im_max
if not keyword_set(FOLLOW) then follow = 0
if n_elements(TYPE) eq 0 then type=1
if not keyword_set(XMINOR) then $
       if !X.MINOR eq 0 then xminor = 5 else xminor = !X.MINOR
if not keyword_set(YMINOR) then $
       if !Y.MINOR eq 0 then yminor = 5 else yminor = !Y.MINOR
if keyword_set(fullw) eq 0 then begin
;if n_elements(nx1) eq 0 then nx1=nint(0.15*!d.x_vsize)/float(!d.x_vsize)
if n_elements(nx1) eq 0 then nx1=nint(0.2*!d.x_vsize)/float(!d.x_vsize)
  ;** Chosen to be the default value;
if n_elements(nx2) eq 0 then nx2=nint(0.98*!d.x_vsize)/float(!d.x_vsize)
  ;** Could be overruled if noaspect is not set;
if n_elements(ny1) eq 0 then ny1=nint(0.10*!d.y_vsize)/float(!d.y_vsize)
if n_elements(ny2) eq 0 then ny2=nint(0.98*!d.y_vsize)/float(!d.y_vsize)
  ;** Could be overruled if noaspect is not set;
endif else begin
if n_elements(nx1) eq 0 then begin
nx1=0. & nx2=1. & ny1=0. & ny2=1. 
endif
 endelse
;**
;** First get the grey-scaled image:
;**
if N_params() lt 3 then im_grey=im
  grey,im_grey,nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,interp=interp,rimage=rimage, $
    greymin=greymin,greymax=greymax,colors=colors,greylog=greylog,xbar=xbar, $
    pscolor=pscolor,noerase=noerase,corner=corner,barshiftx=barshiftx, $
    barshifty=barshifty,barfactor=barfactor,ntoppix=ntoppix,nbotpix=nbotpix, $
    noimage=noimage
;**

;**
;** Get RA and DEC information necessary for subtitle string:
;extast,hdr,para,noparams       ;Extract astrometry from header
;cd=para.cd
;crval=para.crval
crval=sxpar(hdr,'crval*')
;extast,hdr,cd,crpix,crval,noparams       ;Extract astrometry from header
;if noparams LT 0 then $                  ;Does astrometry exist?
 ;     message,'FITS header does not contain astrometry'
;
;crval = crval/!RADEG
;ra_cen=crval(0)*!RADEG & dec_cen=crval(1)*!RADEG
ra_cen=crval(0) & dec_cen=crval(1)
ra_dec = adstring(ra_cen,dec_cen,1)       ;Make a nice string

;** Adjust plotting window so that contour plot will have same dimensional
;** ratio as the image
;**
;xlength = !D.X_VSIZE &  ylength = !D.Y_VSIZE
;ratio=ylength/float(xlength)
xsize = fix(xsize)  &   ysize = fix(ysize)
;xsize1 = xsize-1 & ysize1 = ysize-1
;xratio = xsize/float(ysize)
;yratio = ysize/float(xsize)

pos =[nx1,ny1,nx2,ny2]
if n_elements(xposfac) ne 0 then begin
	nx=(nx2-nx1)*xposfac
	nxc=(nx2+nx1)*0.5
	pos(0)=nxc-0.5*nx
	pos(2)=nxc+0.5*nx
endif
if n_elements(yposfac) ne 0 then begin
	ny=(ny2-ny1)*yposfac
	nyc=(ny2+ny1)*0.5
	pos(1)=nyc-0.5*ny
	pos(3)=nyc+0.5*ny
endif
xmax = pos(2)            ;** added arbitrary

  if n_elements(equi) eq 0 then begin
    xunits = '!6RIGHT ASCENSION (J2000)'
    yunits = 'DECLINATION (J2000)'
     endif else begin
    xunits = 'RIGHT ASCENSION ('+ strtrim(equi,2)+')'
    yunits = 'DECLINATION ('+strtrim(equi,2)+')'
  endelse

;**
;** Get default values of XTITLE, YTITLE, TITLE and SUBTITLE
;**
if not keyword_set(PUTINFO) then putinfo = 0

if n_elements(xtitle) eq 0 then xtitle=xunits

if n_elements(ytitle) eq 0 then ytitle = yunits

if n_elements(title) eq 0 then title = !P.TITLE

if (n_elements(subtitle) eq 0) and (putinfo lt 1) then $
  subtitle = '!6'$ ;CENTER:  R.A. '+ strmid(ra_dec,1,13)+'  DEC ' + strmid(ra_dec,13,13) $
else if n_elements(subtitle) ne 0 then subtitle = subtitle $
  else subtitle = !P.SUBTITLE
;if n_elements(f_color) eq 0 then f_color=!p.color
if n_elements(f_color) eq 0 then begin
	if n_elements(b_c) eq 0 then f_color=!p.color else begin
		f_color=!d.n_colors-1
		c_colors=!d.n_colors-1
	endelse
endif
if n_elements(c_colors) eq 0 then c_colors=!p.color ;black
if n_elements(c_line) eq 0 then c_line=replicate(0,nlevels)
dif=where(c_colors ne f_color,ndif)

;** c_colors are overrunned by f_color if device='ps';
;** thus we need to draw the contours first:
if ndif ne 0 or n_elements(threshold) ne 0 then begin
;** If PS plot output, c_colors=241 (white contours), f_color=0 (black plot).
 if n_elements(threshold) ne 0 then begin
  contour,im,/noerase, $
    POSITION=pos, XSTYLE=5, YSTYLE=5,$
    XTITLE=xtitle, YTITLE=ytitle,TITLE=title, $
    MAX_VALUE=max_value,SUBTITLE = subtitle, XMINOR = xminor, $
    FOLLOW = follow,YMINOR = yminor, $
    LEVELS = levels, NLEVELS = nlevels,c_line=c_line, $
    c_labels=c_labels,spline=spline,c_colors=c_colors,path_info=info,path_xy=xy
  cont_plot,xy,info,im_grey,pos=pos,thre=threshold $
	,clo=c_colors(n_elements(c_colors)-1),chi=c_colors(0)
 endif else $
  contour,im,/noerase, $
    POSITION=pos, XSTYLE=5, YSTYLE=5,$
    XTITLE=xtitle, YTITLE=ytitle,TITLE=title, $
    MAX_VALUE=max_value,SUBTITLE = subtitle, XMINOR = xminor, $
    FOLLOW = follow,YMINOR = yminor, $
    LEVELS = levels, NLEVELS = nlevels,c_line=c_line, $
    c_labels=c_labels,spline=spline,c_colors=c_colors
    c_levels=[1.e22] ;** equivalent to no contour
endif else c_levels=levels

 contour,im,/noerase, $
  POSITION=pos, XSTYLE=5, YSTYLE=5, $
  XTITLE=xtitle, YTITLE=ytitle,TITLE=title, $
  MAX_VALUE=max_value,SUBTITLE = subtitle, XMINOR = xminor, $
  FOLLOW = follow,YMINOR = yminor, $
  LEVELS = c_levels, NLEVELS = nlevels,color=f_color,c_line=c_line, $
  c_labels=c_labels,spline=spline,c_colors=c_colors
;** Run imlabelmm:
if f_color ge 0 then begin
if n_elements(c_colors) gt 1 then ic_colors = c_colors(0)
imlabelmmm,hdr,pos=pos,xtitle=xtitle,ytitle=ytitle,chsize=chsize, $
  c_colors=ic_colors,f_color=f_color,ntickx=ntickx,nticky=nticky, $
  botlabel=botlabel,toplabel=toplabel,xunitlv=xunitlv,yunitlv=yunitlv $
	,leftlabel=leftlabel,ticklen=ticklen
endif 
;**
;**  Write info about the contour plot if desired:
;**
if putinfo GE 1 then begin
  xmax = xmax +0.05
  yshift=0.2
  xyouts,xmax,yshift+0.70,'IMAGE SIZE',/norm,color=f_color
  xyouts,xmax,yshift+0.65,'X: ' + strtrim(xsize,2),/norm,color=f_color
  xyouts,xmax,yshift+0.60,'Y: ' + strtrim(ysize,2),/norm,color=f_color
  minim=min(im_grey(where(im_grey ne 0.)))
  xyouts,xmax,yshift+0.55,'MIN: ' + strtrim(minim,2),/norm,color=f_color
  xyouts,xmax,yshift+0.50,'MAX: ' + strtrim(max(im_grey),2),/norm,color=f_color
  xyouts,xmax,yshift+0.45,strmid(!STIME,0,17),/norm,color=f_color
  xyouts,xmax,yshift+0.40,'CONTOUR LEVELS:',/norm,color=f_color
  for i = 0,(nlevels < 9)-1 do $
    xyouts,xmax,yshift+0.35-0.05*i,string(i,'(i2)') + ':'+string(levels(i)), $
      /NORM,COLOR=F_COLOR
endif
if n_elements(pthicko) ne 0 then begin
	!p.thick=pthicko
endif
if n_elements(pcso) ne 0 then begin
	!p.charsize=pcso
endif
;stop,'End of cont_grey.'

return
end


