;+
; NAME:
;	TVOPLOT
; PURPOSE:
;	Overplot onto the TV device (e.g., use after image_cont or image_cont2)
; CATEGORY:
;	General graphics.
; CALLING SEQUENCE:
;	TVOPLOT, xpos, ypos
; INPUTS:
;       XPOS - vector specifying X coordinates
;       YPOS - vector specifying Y coordinates
;
; KEYWORD PARAMETERS:
;       PSYM = number of symbol to be plotted
;       COLORS = color desired for contour plot. 
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
;-
pro tvoplot, xpos, ypos, PSYM = psym, COLORS = colors
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' TVOPLOT, xpos, ypos, psym=psym, colors=colors]'
  return
endif
;
on_error,2                      ;Return to caller if an error occurs
if (keyword_set(psym) eq 0) then psym=0    ;use 0 as default symbol
;
colorsav = !color               ;save colors to restore later
mx = !d.n_colors-1		;Brightest color
if (keyword_set(colors) eq 0) then $
  colors = [mx,mx,mx,0,0,0]	;color vectors
if !d.name eq 'PS' then colors = mx - colors ;invert line colors for pstscrp
;
px = !x.window * !d.x_vsize	;Get size of window in device units
py = !y.window * !d.y_vsize
swx = px(1)-px(0)		;Size in x in device units
swy = py(1)-py(0)		;Size in Y
;six = float(sz(1))		;Image sizes
;siy = float(sz(2))
;aspi = six / siy		;Image aspect ratio
;aspw = swx / swy		;Window aspect ratio
;f = aspi / aspw			;Ratio of aspect ratios
;
;if (!d.flags and 1) ne 0 then begin	;Scalable pixels?
;  if keyword_set(aspect) then begin	;Retain aspect ratio?
;				;Adjust window size
;	if f ge 1.0 then swy = swy / f else swx = swx * f
;	endif
;
;endif else begin	;Not scalable pixels	
;   if keyword_set(window_scale) then begin ;Scale window to image?
;	swx = six		;Set window size from image
;	swy = siy
;    endif else begin		;Scale window
;	if keyword_set(aspect) then begin
;		if f ge 1.0 then swy = swy / f else swx = swx * f
;		endif		;aspect
;	endelse			;window_scale
;  endelse			;scalable pixels
;
oplot,xpos,ypos,psym=psym,pos=[px(0),py(0), px(0)+swx,py(0)+swy],/dev,$
      c_color=colors
;
!color = colorsav
return
end
