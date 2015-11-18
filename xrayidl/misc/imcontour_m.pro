pro imcontour, im, hdr, TYPE=type, PUTINFO=putinfo, XTITLE=xtitle,  $
      YTITLE=ytitle, NLEVELS = nlevels, MAX_VALUE=max_value, LEVELS=levels, $
      YMINOR = yminor
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
;    IMCONTOUR, im, hdr,[ TYPE=, PUTINFO=, XTITLE = , YTITLE = , NLEVELS= 
;               MAX_VALUE=, LEVELS=, TITLE =, SUBTITLE =, FOLLOW = , XMINOR =,
;               YMINOR = ]
;
; INPUTS:
;    IM - 2-dimensional image array
;    HDR - FITS header associated with IM, string array, must include
;          astrometry keywords.   IMCONTOUR will also look for the
;          OBJECT and IMAGE keywords, and print these if found and the 
;          PUTINFO keyword is set.
; OPTIONAL PLOTTING KEYWORDS:
;    TYPE - the type of astronomical labeling to be displayed.   Either set
;           TYPE = 0 (default), distance to center of the image is
;                    marked in units of Arc seconds, arc minutes, or degrees
;           TYPE = 1 astronomical labeling with Right ascension and 
;                    declination.
;    PUTINFO - If set then IMCONTOUR will add information about the image
;           to the right of the contour plot.  Information includes image
;           name, object, image center, image center, contour levels, and
;           date plot was made
;    The other keywords XTITLE, YTITLE, NLEVELS, MAX_VALUE, LEVELS,
;           TITLE, SUBTITLE, FOLLOW, XMINOR, and YMINOR have the same 
;            meaning as in the CONTOUR procedure.  
; NOTES:
;    (1) The contour plot will have the same dimensional ratio as the input
;        image array
;    (2) To contour a subimage, use HEXTRACT before calling IMCONTOUR
; PROCEDURES USED:
;     CHKIMHD, EXTAST, GETROT, TICPOS, TICLABEL, TIC_ONE, TICS, XYAD
;     RA_GRID, DEC_GRID, ADSTRING
; REVISION HISTORY:
;   Written   W. Landsman   STX                    May, 1989
;   Fixed RA,Dec labeling  W. Landsman             November, 1991
;-
On_error,2                                 ;Return to caller

if N_params() LT 2 then begin             ;Sufficient parameters?
      print,'CALLING SEQUENCE - imcontour,im,hdr,TYPE= ,PUTINFO= '
     return
endif

chkimhd,im,hdr,xsize,ysize      ;Make sure header is appropiate to image
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

if not keyword_set(TYPE) then type=0

if not keyword_set(XMINOR) then $
       if !X.MINOR EQ 0 then xminor = 5 else xminor = !X.MINOR

if not keyword_set(YMINOR) then $
       if !Y.MINOR EQ 0 then yminor = 5 else yminor = !Y.MINOR
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
xsize = fix(xsize)  &   ysize = fix(ysize)
xsize1 = xsize-1 & ysize1 = ysize-1
xratio = xsize/float(ysize)
yratio = ysize/float(xsize)
if ylength*xratio lt xlength then begin
    xmax = 0.15 + 0.8*ylength*xratio/xlength
    pos = [0.15,0.15,xmax,0.95]
endif else  begin
       xmax = 0.95
       pos = [0.15,0.15,xmax,.15+ 0.8*xlength*yratio/ylength]
endelse
;
;
if !X.TICKS GT 0 then xtics = abs(!X.TICKS) else xtics = 8
if !Y.TICKS GT 0 then ytics = abs(!Y.TICKS) else ytics = 8
pixx = xsize/xtics                ;Number of X pixels between tic marks
pixy = ysize/ytics                ;Number of Y pixels between tic marks
getrot,cd,rot,cdelt               ;Get the rotation and plate scale
xmid = xsize1/2.   &   ymid  = ysize1/2.
xyad,hdr,xmid,ymid,ra_cen,dec_cen         ;Get Ra and Dec of image center
ra_dec = adstring(ra_cen,dec_cen,1)       ;Make a nice string
;
; Determine tic positions and labels for the different type of contour plots
;
if type NE 0 then begin                  ;RA and Dec labeling

     xedge = [0,xsize1,0]          ;X pixel values of the four corners
     yedge = [0,0,ysize1]          ;Y pixel values of the four corners
     xy2ad,xedge,yedge,cd,crpix,crval,a,d    ;RA and Dec of the corners
     a = a*!RADEG   &  d = d*!RADEG
     pixx = xsize/xtics                ;Number of X pixels between tic marks
     pixy = ysize/ytics                ;Number of Y pixels between tic marks
     tics,a(0),a(1),xsize,pixx,raincr,/RA  ;Find an even increment for RA
     tics,d(0),d(2),ysize,pixy,decincr    ;Find an even increment for Dec
     tic_one,a(0),pixx,raincr,botmin,xtic1,/RA     ;Position of first RA tic
     tic_one,d(0),pixy,decincr,leftmin,ytic1       ;Position of first Dec tic
     nx = fix((xsize1-xtic1-1)/pixx)                 ;Number of X tic marks
     ny = fix((ysize1-ytic1-1)/pixy)                 ;Number of Y tic marks
     ra_grid = (botmin + findgen(nx+1)*raincr/4.)/!RADEG
     dec_grid = (leftmin + findgen(ny+1)*decincr/60.)/!RADEG
     ticlabels, botmin, nx+1, raincr, xlab, /RA, DELTA=1
     ticlabels, leftmin, ny+1, decincr, ylab,DELTA=1
     xpos = cons_ra( ra_grid,0,cd,crpix,crval )
     ypos = cons_dec( dec_grid,0,cd,crpix,crval )
     xunits = 'RIGHT ASCENSION'
     yunits = 'DECLINATION'

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
;
; Get default values of XTITLE, YTITLE, TITLE and SUBTITLE
;
if not keyword_set(PUTINFO) then putinfo = 0

if N_elements(xtitle) EQ 0 then $
  if !X.TITLE eq '' then xtitle = xunits else xtitle = !X.TITLE

if N_elements(ytitle) EQ 0 then $
  if !Y.TITLE eq '' then ytitle = yunits else ytitle = !Y.TITLE

if N_elements(title) EQ 0 then title = !P.TITLE

if (N_elements(subtitle) EQ 0) and (putinfo LT 1) then $
    subtitle = 'CENTER:  R.A. '+ strmid(ra_dec,1,13)+'  DEC ' + $
               strmid(ra_dec,13,13) $
    else subtitle = !P.SUBTITLE
;
contour,im, $
         XTICKS = nx, YTICKS = ny, POSITION=pos, XSTYLE=1, YSTYLE=1,$
         XTICKV = xpos, YTICKV = ypos, XTITLE=xtitle, YTITLE=ytitle, $
         XTICKNAME = xlab, YTICKNAME = ylab, TITLE = title, $
         LEVELS = levels, NLEVELS = nlevels, MAX_VALUE=max_value, $
         SUBTITLE = subtitle, FOLLOW = follow,XMINOR = xminor, $
         YMINOR = yminor
; 
;  Write info about the contour plot if desired
;
if putinfo GE 1 then begin
   xmax = xmax +0.01
   object = sxpar(hdr,'OBJECT')
   if !ERR ne -1 then xyouts,xmax,0.95,object,/norm
   name = sxpar(hdr,'IMAGE')
   if !ERR ne -1 then xyouts,xmax,0.90,name,/norm
   xyouts,xmax,0.85,'CENTER:',/NORM
   xyouts,xmax,0.80,'R.A. '+ strmid(ra_dec,1,13),/NORM
   xyouts,xmax,0.75,'DEC '+  strmid(ra_dec,13,13),/NORM
   xyouts,xmax,0.70,'IMAGE SIZE',/norm
   xyouts,xmax,0.65,'X: ' + strtrim(xsize,2),/norm
   xyouts,xmax,0.60,'Y: ' + strtrim(ysize,2),/norm
   xyouts,xmax,0.50,strmid(!STIME,0,17),/norm
   xyouts,xmax,0.40,'CONTOUR LEVELS:',/norm
   for i = 0,(nlevels < 6)-1 do $
      xyouts,xmax,0.35-0.05*i,string(i,'(i2)') + ':'+string(levels(i)),/NORM 
endif
return                                          
end                                         
