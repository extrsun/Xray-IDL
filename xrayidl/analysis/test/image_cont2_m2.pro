;+
; NAME:
;	IMAGE_CONT2
; PURPOSE:
;	Overlay an image and a contour plot.
; CATEGORY:
;	General graphics.
; CALLING SEQUENCE:
;	IMAGE_CONT2, a[, a2, xpos, ypos]
; INPUTS:
;	A = 2 dimensional array to display as a greyscale plot
;       A2 = another 2 dimensional array to overlay as a contour plot
;            if not specified, then A2 = A
;       XPOS - vector specifying X coordinates for contour surface
;       YPOS - vector specifying Y coordinates for contour surface
;
; KEYWORD PARAMETERS:
;	/WINDOW_SCALE = set to scale the window size to the image size,
;		otherwise the image size is scaled to the window size.
;		Ignored when outputting to devices with scalable pixels.
;	/ASPECT = set to retain image's aspect ratio.  Assumes square
;		pixels.  If /WINDOW_SCALE is set, the aspect ratio is
;		retained.
;	/INTERP = set to bi-linear interpolate if image is resampled.
;       /LEVELS = set to levels desired for contour overlay. If not set,
;                 6 equally spaced levels are drawn
;       /NLEVELS = number of levels desired for contour overlay. Superceded
;                  by LEVELS if LEVELS is set. If LEVELS is not set, equally
;                  spaced contours will be drawn.
;       /COLORS = color desired for contour plot. 
; OUTPUTS:
;	No explicit outputs.
; COMMON BLOCKS:
;	none.
; SIDE EFFECTS:
;	The currently selected display is affected.
; RESTRICTIONS:
;	None that are obvious.
; PROCEDURE:
;	If the device has scalable pixels then the image is written over
;	the plot window.
; MODIFICATION HISTORY:
;     taken from IMAGE_CONT Sep 1991 (GAR)
;     modified to allow keywords levels, nlevels, and color 20 Nov 1991 (GAR)
;-
pro image_cont2, a, a2, xpos, ypos, WINDOW_SCALE = window_scale, $
    ASPECT = aspect, INTERP = interp, LEVELS = levels, NLEVELS = nlevels, $
    COLORS = colors
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' IMAGE_CONT2, a, [a2, xpos, ypos, window_scale=window_scale,'
  print,'              aspect=aspect, interp=interp, levels=levels,'
  print,'              nlevels=nlevels, colors=colors]'
  return
endif
;
on_error,2                      ;Return to caller if an error occurs
sz = size(a)			;Size of image
if sz(0) lt 2 then message, 'Parameter not 2D'
;
if (keyword_set(levels) eq 0) then begin
  if (npar eq 1) then begin
    amax = max(a)
    amin = min(a)
  endif else begin
    amax = max(a2)
    amin = min(a2)
  endelse
  if (keyword_set(nlevels) eq 0) then nlevels = 6
  deltaa = (amax - amin)/(nlevels-1.)
  levels = amin + findgen(nlevels)*deltaa
endif  
if (!debug gt 1) then stop,'Stopping after levels have been set'
;
colorsav = !color               ;save colors to restore later
mx = !d.n_colors-1		;Brightest color
if (keyword_set(colors) eq 0) then $
  colors = [mx,mx,mx,0,0,0]	;color vectors
if !d.name eq 'PS' then colors = mx - colors ;invert line colors for pstscrp
;
scsquare                        ;needed to make things work out right
                               ;set window used by contour
contour,[[0,0],[1,1]],/nodata, xstyle=4, ystyle = 4
;
px = !x.window * !d.x_vsize	;Get size of window in device units
py = !y.window * !d.y_vsize
swx = px(1)-px(0)		;Size in x in device units
swy = py(1)-py(0)		;Size in Y
six = float(sz(1))		;Image sizes
siy = float(sz(2))
aspi = six / siy		;Image aspect ratio
aspw = swx / swy		;Window aspect ratio
f = aspi / aspw			;Ratio of aspect ratios
;
if (!debug gt 1) then stop,'before flag'
if (!d.flags and 1) ne 0 then begin	;Scalable pixels?
  if keyword_set(aspect) then begin	;Retain aspect ratio?
				;Adjust window size
	if f ge 1.0 then swy = swy / f else swx = swx * f
	endif
;
  tvscl,a,px(0),py(0),xsize = swx, ysize = swy, /device
if (!debug gt 1)  then stop,'after tvscl'
;
endif else begin	;Not scalable pixels	
   if keyword_set(window_scale) then begin ;Scale window to image?
	tvscl,a,px(0),py(0)	;Output image
	swx = six		;Set window size from image
	swy = siy
    endif else begin		;Scale window
	if keyword_set(aspect) then begin
		if f ge 1.0 then swy = swy / f else swx = swx * f
		endif		;aspect
	tv,poly_2d(bytscl(a),$	;Have to resample image
		[[0,0],[six/swx,0]], [[0,siy/swy],[0,0]],$
		keyword_set(interp),swx,swy), $
		px(0),py(0)
	endelse			;window_scale
if !debug gt 1 then stop,'near poly'
  endelse			;scalable pixels
;
if !debug gt 1 then stop,'outside the if'
if (npar gt 1) then $
  contour,a2,/noerase,/xstyle,/ystyle,$	;Do the contour
	   pos = [px(0),py(0), px(0)+swx,py(0)+swy],/dev,$
	c_color =  colors, levels = levels
if (npar eq 1) then $
  contour,a,/noerase,/xstyle,/ystyle,$	;Do the contour
	   pos = [px(0),py(0), px(0)+swx,py(0)+swy],/dev,$
	c_color =  colors, levels = levels
;
!color = colorsav
return
end
