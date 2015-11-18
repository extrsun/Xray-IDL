;+
; NAME:
;	GREY
; PURPOSE:
;	Plot grey scale image in normalized coordinates. Overlaid contours 
; can be obtained by calling cont_grey instead
;
; CATEGORY:
;	General graphics.
; CALLING SEQUENCE:
;	GREY, a
; INPUTS:
;	A = 2 dimensional array to display as a greyscale plot
;
; KEYWORD PARAMETERS:
;	/nx1,/nx2,/ny1,/ny2  = The coordinates of the lower left and the upper
;                            right coners of the image
;	/NOASPECT = set to retain image's dimension ratio.  Otherwise,
;	the image's aspec ratio is retained. Assumes square
;		pixels.  
;	/INTERP = set to bi-linear interpolate if image is resampled.
;       /COLORS = color desired for contour plot. 
;	rimage - if non zero, the grey scale will be generated with a
;	random generator (using plot_rimage). The value of rimage is used
;	as the maximum expected number points per pixel
;
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
;
; OUTPUTS:
;	No explicit outputs.
; COMMON BLOCKS:
;	none.
; SIDE EFFECTS:
;	The currently selected display is affected.
; RESTRICTIONS:
;	None that are obvious.
; PROCEDURE:
;       Use normalized coordinates instead of device coordinates
;	the plot window.
; SUBROUTINES CALLED:
; 	bscale,index_conv
; MODIFICATION HISTORY:
;     writen on Aug 19 1992 (WQD) 
; 	add the option to use the random number generator for a better
;	look of an image, Aug. 1993 (wqd)
; added keywords, pscolor, for a color postscript file
; keywords: barshiftx, barshifty, barfactor are added 
; wqd, Jan 7, 1994
;-
pro grey, aa, nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2, NOASPECT = noaspect, INTERP = interp, COLORS  = colors, device=device,rimage=rimage,greymin=greymin,greymax=greymax,greylog=greylog,xbar=xbar,pscolor=pscolor,noerase=noerase,corner=corner,barshiftx=barshiftx,barshifty=barshifty,barfactor=barfactor,ntoppix=ntoppix,nbotpix=nbotpix,noimage=noimage,b_c=b_c
common colors,r_orig,g_orig,b_orig,r_curr,g_curr,b_curr
;
if (n_params() eq 0) then begin
print,' grey, aa, [nx1,nx2,ny1,ny2,noaspect,interp,colors,device,rimage'
print,',greymin,greymax,greylog,xbar,pscolor,noerase,corner'
print,',barshiftx,barshifty,barfactor,ntoppix,nbotpix,b_c]'
Return
endif
;

; set background color if keyword b_c is given
if n_elements(b_c) ne 0 then begin
	polyfill,[0,1,1,0],[0.,0.,1,1],color=b_c,/norm
	noerase=1
endif
a=aa
if n_elements(greylog) eq 0 then greylog=0
if greylog eq 1 then a=alog10(a > 1.e-10)
if greylog eq 2 then a=sqrt(a > 1.e-10)

if n_elements(greymin) eq 0 then begin 
	greymin=min(a)
endif else begin
	if  greylog eq 1 then greymin=alog10(greymin)
	if  greylog eq 2 then greymin=sqrt(greymin)
	c=where(a ne 0,nc)
	if n_elements(nc) ne 0 then a(c)=a(c) > greymin 
endelse
if n_elements(greymax) eq 0 then begin 
	greymax=max(a)
endif else begin
	if  greylog eq 1 then greymax=alog10(greymax)
	if  greylog eq 2 then greymax=sqrt(greymax)
	a=a < greymax
endelse
if n_elements(nx1) eq 0 then nx1=0. ; ;chosen to be the default value
if n_elements(nx2) eq 0 then nx2=1.  ;could be overruled if noaspect is not set
if n_elements(ny1) eq 0 then ny1=0.
if n_elements(ny2) eq 0 then ny2=1.  ;could be overruled if noaspect is not set
if keyword_set(device) ne 0 then begin
	dname=!d.name
	set_plot,device
	device,/color
endif
if n_elements(ntoppix) eq 0 then ntoppix=1 
	;reserved the highest pixel value for other uses (e.g., contours)
if n_elements(nbotpix) eq 0 then nbotpix=1 ;to make the background not white
;
;on_error,2                      ;Return to caller if an error occurs
sz = size(a)			;Size of image
if sz(0) lt 2 then message, 'Parameter not 2D'
;  
colorsav = !color               ;save colors to restore later
mx = !d.n_colors-1		;Brightest color
if (!d.name ne 'PS' or keyword_set(pscolor) ne 0) $
	and n_elements(r_curr) ne 0 then begin
	tvlct,r_orig,g_orig,b_orig
endif else begin
	colors=indgen(!d.n_colors-ntoppix) 
	tvlct,colors,colors,colors
;	loadct_self,14
endelse
contour,[[0,0],[1,1]],/nodata, xstyle=4, ystyle = 4,noerase=noerase
;
swx=(nx2-nx1)*!d.x_vsize        
swy=(ny2-ny1)*!d.y_vsize
dxa=nx1*!d.x_vsize
dya=ny1*!d.y_vsize
six = float(sz(1))		;Image sizes
siy = float(sz(2))
aspi = six / siy		;Image aspect ratio
aspw = float(swx) / swy		;Window aspect ratio
f = aspi / aspw			;Ratio of aspect ratios
 ;
  if keyword_set(noaspect) ne 1 then begin	;Retain aspect ratio?
				;Adjust window size
	if f ge 1.0 then swy = swy / f else swx = swx * f
  endif
nx2=nx1+swx/!d.x_size
ny2=ny1+swy/!d.y_size
corner=[nx1,nx2,ny1,ny2]
if keyword_set(noimage) ne 0 then return
if n_elements(rimage) ne 0 then begin
	plot_rimage,bscale(a,greymin,greymax,ntoppix=ntoppix,nbotpix=nbotpix) $
		*(rimage/238.),corner
	goto,done
endif
if (!d.flags and 1) ne 0 then begin	;Scalable pixels?
  	if !d.name eq 'PS' and keyword_set(pscolor) eq 0 then begin
		tv,!d.n_colors-ntoppix-bscale(a,greymin,greymax $
		,ntoppix=ntoppix,nbotpix=nbotpix),dxa,dya, $
		xsize = swx, ysize = swy, /device
	endif else begin
		tv,bscale(a,greymin,greymax,ntoppix=ntoppix,nbotpix=nbotpix) $
		,dxa,dya,xsize = swx, ysize = swy, /device
	endelse
endif else begin	;Not scalable pixels	
	tv,poly_2d(bscale(a,greymin,greymax,ntoppix=ntoppix,nbotpix=nbotpix), $
		[[0,0],[six/swx,0]], [[0,siy/swy],[0,0]], $
		keyword_set(interp),swx,swy),dxa,dya
endelse			

if n_elements(barshiftx) eq 0 then barshiftx=0
if n_elements(barshifty) eq 0 then barshifty=0
if n_elements(barfactor) eq 0 then barfactor=1.
if barfactor ne 0 then begin ; get the color bar
 bsx=barshiftx*!d.x_size
 bsy=barshifty*!d.x_size

 if !d.name eq 'PS' then begin
   if n_elements(xbar) ne 0 then begin
	index_conv,lindgen((!d.n_colors-(ntoppix+nbotpix))*20) $
		,[!d.n_colors-(ntoppix+nbotpix),20],index
	index=index+nbotpix
	bar=fltarr(!d.n_colors-(ntoppix+nbotpix),20)
	if keyword_set(pscolor) eq 0 then $
		bar(*)=(!d.n_colors-(ntoppix+nbotpix)-index(0,*)) else $
		bar(*)=index(0,*)
	tv,bar,bsx+dxa,bsy+2+dya+swy,xsize =barfactor*swx, ysize = 0.05*swy, $
		 /device 
   endif else begin
	index_conv,indgen((!d.n_colors-(ntoppix+nbotpix))*20) $
		,[20,!d.n_colors-(ntoppix+nbotpix)],index
	index=index+nbotpix
	bar=fltarr(20,!d.n_colors-(ntoppix+nbotpix))
	if keyword_set(pscolor) eq 0 then $
		bar(*)=(!d.n_colors-(ntoppix+nbotpix)-index(1,*)) else $
		bar(*)=index(1,*)
	tv,bar,bsx+dxa+swx,bsy+dya+2,xsize =0.05*swx, ysize =barfactor*swy,$
		 /device 

   endelse
 endif else begin
   if n_elements(xbar) ne 0 then begin
	iswx=fix(swx*barfactor)
	factor=(!d.n_colors-(ntoppix+nbotpix))/(swx)
	index_conv,lindgen(iswx*20),[iswx,20],index
	bar=fltarr(iswx,20)
	bar(*)=index(0,*)*factor+nbotpix
	tv,bar,bsx+dxa,bsy+2+dya+swy
   endif else begin
	iswy=fix(swy*barfactor)
	factor=(!d.n_colors-(ntoppix+nbotpix))/(swy)
	index_conv,indgen(iswy*20),[20,iswy],index
	bar=fltarr(20,iswy)
	bar(*)=index(1,*)*factor+nbotpix
	tv,bar,bsx+2+dxa+swx,bsy+dya
   endelse
 endelse
endif
	
;
done:
if !debug gt 1 then stop
!color = colorsav
if keyword_set(device) ne 0 then begin
	device,/close
	set_plot,dname
endif
return
end
