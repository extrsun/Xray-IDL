pro cont_grey, im, hdr, im_grey,true=true,cor=cor,fullw=fullw $
,greymin=greymin,greymax=greymax,greylog=greylog,mr=mr $
,levels=levels,nocont=nocont,noimage=noimage,noerase=noerase $
,pscolor=pscolor,f_color=f_color,c_colors=c_colors $
,c_thick=c_thick,c_labels=c_labels,c_line=c_line,barscale=barscale $
,pthick=pthick,pcs=pcs,b_c=b_c,barsclvertical=barsclvertical $
,barshiftx=barshiftx,barshifty=barshifty,barfactor=barfactor $
,threshold=threshold,thlo=thlo,thhi=thhi,TYPE=type $
, PUTINFO=putinfo, XTITLE=xtitle,YTITLE=ytitle, NLEVELS = nlevels, $
  YMINOR = yminor,follow=follow,colors=colors,interp=interp, $
  subtitle=subtitle,equi=equi,rimage=rimage,xbar=xbar, $
  spline=spline,ntoppix=ntoppix,nbotpix=nbotpix, $
  ntickx=ntickx,nticky=nticky,botlabel=botlabel,toplabel=toplabel, $
  xunitlv=xunitlv,yunitlv=yunitlv,xposfac=xposfac $
 ,yposfac=yposfac,leftlabel=leftlabel,TITLE=title,ticklen=ticklen,pos=pos $
 ,box_color=box_color,noscale=noscale,defcor=defcor,notick=notick,thick=thick,chsize=chsize
;+
; NAME:
;    CONT_GREY
; PURPOSE:
;    Plot contours and/or image(s) with or without astronomical
;    coordinates. 
;
;    Functions: 1) contour and/or greyscale plot for the same image
;		2) contour overplay on a different image
;		3) RGB color plot with or without contours
;    Notes: the greyscale can also plotted in color by using loadct(_self)
;	 or in random counts generated according to the image intensity 
;	(rimage).
;	Almost every aspect of the plot can be customized.
;
; INPUTS:
;    IM      = 2-dimensional image array, or a stack of three 
;		2-dimensional image arrays if true=3 is used and no im_grey
;		is provided
;    HDR     = FITS header associated with IM, string array, must include
;              astrometry keywords.   CONT_GREY will also look for the
;              OBJECT and IMAGE keywords, and print these if found and the
;              PUTINFO keyword is set.
;    IM_GREY = 2-dimensional image array or a stack of three 
;		2-dimensional image arrays (if true=3 is given). 
;		If given, Im will be used only for plotting contours
;              grey-scaled plot.
;
; OPTIONAL PLOTTING KEYWORDS:
;
; true - if true=3 is set, im_grey or im (if im_grey is not provided) needs
;	to be a stack of three 2-dimensional image arrays for an rgb plot.
; cor   = if set, the output gives the normlized coordinates of
;            xmin, xmax, ymin, ymax of the image;
; fullw = a simple keyword to fill the image over the window. But it
;	will be overrided by cor if given.
; greymin, greymax  = the image values as the lower and upper limits of the 
;		grey plot;
; greylog = if =1; the greyscale will be plotted logrithmatically
;	    if =2; plotted in square root
; 	    if = other values, plotted linearlly
; mr - the margin between the image and the frame (def: rm=0.02)
;	set mr=0 to remove the margin
; levels - a vector of im values for plotting contour levels
; nocont - if set, no contours will be plotted
; noimage - if set, the greyscale image will not be plotted
; noerase - if set, the previous plot in the window be overplotted without 
;		erasing
; pscolor  = a keyword if set indicates a color postscript file is to
; f_color  = the color of the plot frame. the default=!p.color (e.g., 0);
; c_colors = single value or a vector containing the color index(es) used
;            to draw contours;
; c_thick, c_labels, c_line - thinkness, contour level label, line style of
;		the contours (as in the contour program) be produced;
; pthick,pcs - plot thickness and character size
; b_c - the background color code.
; barshiftx, barshifty, barfactor
;          = the x and y shifts (in normalized coordinates) and multiple
;            size factor of the color bar (default=1; the same linear size
;            as the figure). These three parameters are used to locate the
;            bar in the figure;
; threshold - the greyscale image value for plotting contours in
;		different colors.
; thlo,thhi - the tolerance factor for the threshold to avoid too frequent
;		change of color, e.g., thlo=0.5,thhi=1.5?
; ntoppix  = the number of top color indexes in the color table used for
;            other purposes (e.g., drawing  the frame of the figure).  If
;            the original color table is expanded to more levels (because
;            of device differences), ntoppix may need to be increased
;            accordingly.
; yunitlv - how far below the x-axis to place the x-axis annotation
;	   in normalized units of the space from the x-axis to the
;	   edge of the plot device; default is .1;
; xunitlv - how far to the left of the y-axis to place the y-axis
;	   annotation in normalized units of the space from the
;	   y-axis to the edge of the plot device; default is .15;
; noscale - if set, the image is not scaled by bscale.
; equi     = the equinox of the coordinate system for labeling. Def = J2000;
; colors   = gray scale color index (have not been tested);
; 
; spline   = a small value (e.g., 0.001) will give better looking contours
;		(not sure if it works)
; defcor - if set, def cor will be used.
;
;    TYPE = the type of astronomical labeling to be displayed.   Either set
;           TYPE = 0 distance to center of the image is marked in units of
;                    arcseconds, arcminutes, or degrees;
;           TYPE = 1 (default) astronomical labeling with Right ascension and
;                    declination.
;    PUTINFO = If set then CONT_GREY will add information about the image
;              to the right of the contour plot.  Information includes image
;              name, object, image center, image center, contour levels, and
;              date plot was made.
; thick - the thickness of the outer box (def =2)
;
;  Keywords XTITLE, YTITLE, NLEVELS, LEVELS,
;  TITLE, SUBTITLE, FOLLOW, XMINOR, and YMINOR have the same
;  meaning as in the CONTOUR procedure.
;
; NOTES:
; (1) The contour plot will have the same dimensional ratio as the input
;        image array.
;
; (2) To get a contour colors different from that of the axis, the keyword
; overrun problem of c_colors that is responsible for
; both axis and contours is solved by calling contour a second time.
; For device 'ps' c_color=0 (black), =!d.n_colors (white), and
; for device 'x', it is the reverse. wqd, oct 24 1993.
;
; RESTRICTIONS:
;	The rotation angle must be 0 (north is up) in the image.
;	IMLABELMMM has not been tested for REALLY large areas, such
;	as those where constant RA grid lines start to wrap around.
;
; REVISION HISTORY:
;   Extracted and expanded by wqd (Aug 1992) by including GREY
; added keywords, pscolor, for a color postscript file
; keywords: barshiftx, barshifty, barfactor are added
; wqd, Jan 7, 1994
;
; add a call to cont_plot for ploting contours with different colors for
; low and high intensity grey-scale regions. wqd, Sept. 13, 1997
; modified by wqd (March 8, 2000) to allow for plotting a margin between
; the frame and the image
;
; re-organized the comments and made changes on the keywords fullw and cor
; wqd (1/1/2002)
; modified by sw  (8/13/2013), add keyword barscale to control the ploting 
;      of the scale of the color-bar
; modified by sw (8/29/2013), add keyword "barbrdfac" to broaden the width 
;      of the color bar.
; modified by sw, Sep 05, 2013: add the keyword "barsclvertical" to 
;   make a special vertical labels of the scale of the color bar to 
;   fit the PV diagram plotting
;-

if N_params() lt 2 then begin             ;Sufficient parameters?
  print,'CALLING SEQUENCE - cont_grey,im,hdr, [im_grey, '
  print,'TYPE=type, PUTINFO=putinfo, XTITLE=xtitle,YTITLE=ytitle'
  print,',NLEVELS = nlevels,LEVELS=levels,'
  print,'YMINOR = yminor,follow=follow,colors=colors,interp=interp'
  print,',subtitle=subtitle,cor=cor,equi=equi,rimage=rimage'
  print,',c_line=c_line,greymin=greymin,greymax=greymax,f_color=f_color,greylog=greylog'
  print,',xbar=xbar,c_labels=c_labels,barscale=barscale,'
  print,',spline=spline,c_colors=c_colors,pscolor=pscolor,noerase=noerase'
  print,',barshiftx=barshiftx,barshifty=barshifty,barfactor=barfactor'
  print,',ntoppix=ntoppix,nbotpix=nbotpix,noimage=noimage'
  print,'ntickx=ntickx,nticky=nticky,barbrdfac=barbrdfac,'
  print,',pthick=pthick,pcs=pcs,xunitlv=xunitlv,yunitlv=yunitlv'
  print,',xposfac=xposfac,yposfac=yposfac,botlabel=botlabel,toplabel=toplabel'
  print,',fullw=fullw,b_c=b_c,pos=pos,barsclvertical=barsclvertical'
  print,',TITLE=title,ticklen=ticklen,pos=pos,threshold=threshold,thlo=thlo,'
  print,'thhi=thhi,box_color=box_color,mr=mr,c_thick=c_thick,nocont=nocont,'
  print,'true=true,defcor=defcor,notick=notick,thick=thick'
 return
endif

if n_elements(c_labels) eq 0 then c_labels=0
if n_elements(chsize) eq 0 then chsize=1
if n_elements(spline) eq 0 then spline=0.005
if N_params() lt 3 then im_grey=im
if keyword_set(true) eq 0 then begin
 check_fits,im,hdr,dimen,/notype     ;Make sure header is appropiate to image
 xsize=dimen(0)
 ysize=dimen(1)
endif else begin
 sz=size(im_grey)
 if sz(0) ne 3 then begin
	print,'The image needs to be 3-D to get an rgb plot
	return
 endif
 xsize=sz(1)
 ysize=sz(2)
endelse 
;**
;** Set defaults if keywords not set:
;**
if n_elements(pthick) ne 0 then begin
	pthicko=!p.thick
	!p.thick=pthick
endif
if n_elements(pcs) ne 0 then begin
	pcso=!p.charsize
	!p.charsize=pcs
endif
if n_elements(c_thick) eq 0 then c_thick=!p.thick
im_max = max( im, MIN=im_min )         ;** Image MAX and MIN values;
if not keyword_set(LEVELS) then  $     ;** Default is 6 equally spaced levels;
   levels = im_min + (findgen(6)+1)*(float(im_max)-im_min)/7.
if not keyword_set(NLEVELS) then  $        ;Default is NLEVELS = 6
   nlevels = n_elements(levels)
if not keyword_set(FOLLOW) then follow = 0
if n_elements(TYPE) eq 0 then type=1
if not keyword_set(XMINOR) then $
       if !X.MINOR eq 0 then xminor = 5 else xminor = !X.MINOR
if not keyword_set(YMINOR) then $
       if !Y.MINOR eq 0 then yminor = 5 else yminor = !Y.MINOR

if n_elements(cor) eq 0 or keyword_set(defcor) then begin
	if keyword_set(fullw) eq 0 then $
	 cor=nint([0.2,0.95,0.1,0.98]*!d.x_vsize)/float(!d.x_vsize) $
	 else cor=[0.,1.,0.,1.-1./512.] 
endif
if n_elements(mr) eq 0 then begin
	if keyword_set(fullw)  then mr=0 else mr=0.02
endif
if n_elements(barshiftx) eq 0 then if mr eq 0 then barshiftx=0.
;**
;** First get the grey-scaled image:
;**

;if keyword_set(noerase) eq 0 then $
  ; print,greymax,greymin,greylog
  grey,im_grey,interp=interp,rimage=rimage,barbrdfac=barbrdfac, $
    greymin=greymin,greymax=greymax,colors=colors,greylog=greylog,xbar=xbar, $
    pscolor=pscolor,cor=cor,barshiftx=barshiftx,barscale=barscale, $
    barshifty=barshifty,barfactor=barfactor,ntoppix=ntoppix,nbotpix=nbotpix, $
    noimage=noimage,noscale=noscale,true=true,noerase=noerase, $
    barsclvertical=barsclvertical
;**
xsize = fix(xsize)  &   ysize = fix(ysize)
pos = [cor(0),cor(2),cor(1),cor(3)]
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

if n_elements(f_color) eq 0 then begin
	;set the frame color according to the choice of the background color
	;if n_elements(b_c) eq 0 then f_color=!p.color else $
		f_color=!d.n_colors-1
endif
if n_elements(c_colors) eq 0 then c_colors=!p.color 
	
if n_elements(c_line) eq 0 then c_line=replicate(0,nlevels)
dif=where(c_colors ne f_color,ndif)

if keyword_set(nocont) eq 0 then begin
;** c_colors are overrunned by f_color if device='ps';
;** thus we need to draw the contours first:
if ndif ne 0 or n_elements(threshold) ne 0 then begin
 if n_elements(threshold) ne 0 then begin
	if !d.name eq 'PS' then c_colors=reverse(c_colors)
  contour,im,/noerase, $
    POSITION=pos, XSTYLE=5, YSTYLE=5, XTITLE=xtitle, YTITLE=ytitle,$
    TITLE=title, SUBTITLE = subtitle, XMINOR = xminor, $
    FOLLOW = follow,YMINOR = yminor, $
    LEVELS = levels, NLEVELS = nlevels,c_line=c_line,c_thick=c_thick, $
    c_labels=c_labels,spline=spline,c_colors=c_colors,path_info=info,path_xy=xy
  if n_elements(xy) ne 0 then $
  cont_plot,xy,info,im_grey,pos,thre=threshold,thlo=thlo,thhi=thhi $
	,clo=c_colors(n_elements(c_colors)-1),chi=c_colors(0),c_line=c_line
 endif else $
  contour,im,/noerase, $
    POSITION=pos, XSTYLE=5, YSTYLE=5,$
    XTITLE=xtitle, YTITLE=ytitle,TITLE=title, $
    SUBTITLE = subtitle, XMINOR = xminor, $
    FOLLOW = follow,YMINOR = yminor, $
    LEVELS = levels, NLEVELS = nlevels,c_line=c_line,c_thick=c_thick, $
    c_labels=c_labels,spline=spline,c_colors=c_colors
    c_levels=[1.e22] ;** equivalent to no contour
endif else c_levels=levels

 contour,im,/noerase, POSITION=pos, XSTYLE=5, YSTYLE=5, XTITLE=xtitle, $
  YTITLE=ytitle,TITLE=title, SUBTITLE = subtitle, XMINOR = xminor, $
  FOLLOW = follow,YMINOR = yminor, LEVELS = c_levels, NLEVELS = nlevels, $
  color=f_color,c_line=c_line,c_thick=c_thick, $
  c_labels=c_labels,spline=spline,c_colors=c_colors
endif 
;** Run imlabelmm:
if f_color ge 0 then begin
 if n_elements(c_colors) gt 1 then ic_colors = c_colors(0)
 if mr gt 0 then begin
	mdx=mr*(pos(2)-pos(0))
	hdrm=hdr
	xmo=sxpar(hdr,'NAXIS1')
	xd=xmo*mr
	sxaddpar,hdrm,'NAXIS1',xmo+2.*xd
	sxaddpar,hdrm,'NAXIS2',sxpar(hdr,'NAXIS2')+2.*xd
	sxaddpar,hdrm,'crpix1',sxpar(hdr,'crpix1')+xd
	sxaddpar,hdrm,'crpix2',sxpar(hdr,'crpix2')+xd
	mdy=mdx*!d.x_vsize/double(!d.y_vsize)
	posm=[pos(0)-mdx,pos(1)-mdy,pos(2)+mdx,pos(3)+mdy] 
 endif else begin
	hdrm=hdr
	posm=pos
 endelse
;print,'pos, posm = ',pos, posm
 imlabelmmm,hdrm,pos=posm,xtitle=xtitle,ytitle=ytitle,chsize=chsize, $
  c_colors=ic_colors,f_color=f_color,ntickx=ntickx,nticky=nticky, $
  botlabel=botlabel,toplabel=toplabel,xunitlv=xunitlv,yunitlv=yunitlv $
	,leftlabel=leftlabel,ticklen=ticklen,type=type,notick=notick,thick=thick
endif 

;else begin
; posm=pos
; if n_elements(box_color) eq 0 then box_color=!P.COLOR
; pos=double(pos)
; nx1 = posm(0) & ny1 = posm(1)
; nx2 = posm(2) & ny2 = posm(3)
; plots, [nx1,nx1], [ny1,ny2], /normal, color=box_color
; plots, [nx1,nx2], [ny1,ny1], /normal, color=box_color
; plots, [nx2,nx2], [ny1,ny2], /normal, color=box_color
; plots, [nx1,nx2], [ny2,ny2], /normal, color=box_color
;endelse

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
if !debug eq 2 then stop,'End of cont_grey.'

return
end


