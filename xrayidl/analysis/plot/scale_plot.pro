pro scale_plot,corner,hdr,nxc,nyc,amscale,text,thick=thick,txoff=txoff,tyoff=tyoff,charsize=charsize,color=color
;-
; plot a scale bar and text in an existing plot (using cont_grey)
; 
; corner - a vector containing xmin, xmax, ymin,ymax of the image (normalized
;	coordinates from cont_grey.pro
; hdr - fits header
; nxc, nyc - the normalized coordinates within the image plot
; amscale - the scale in units of arcmin
; text - the text to be plotted
; thick - the thickness of the bar, default = 1
; txoff, tyoff - the offset of the text from nxc, nyc in the same units
;		defaults: txoff=0 and tyoff=0.02
; color - color of the bar
;
; writen by WQD 6/22/00
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - scale_plot,corner,hdr,nxc,nyc,amscale,text,thick=thick,txoff=txoff,tyoff=tyoff,charsize=charsize,color=color'
return
endif
if n_elements(thick) eq 0 then thick=2
if n_elements(txoff) eq 0 then  txoff=0.
if n_elements(tyoff) eq 0 then  tyoff=0.02
if n_elements(color) eq 0 then begin
			if !d.name eq 'X' then color=!d.n_colors-1 $
			else color=0
endif
naxis=sxpar(hdr,'naxis*')
xarcmin=naxis(0)*abs(sxpar(hdr,'cdelt1'))*60.
yarcmin=naxis(1)*abs(sxpar(hdr,'cdelt2'))*60.
namscale=amscale/xarcmin*(corner(1)-corner(0))
xmid=corner(0)+(corner(1)-corner(0))*nxc
ymid=corner(2)+(corner(3)-corner(2))*nyc
plots,[-1,1]*0.5*namscale+xmid,ymid,/normal,thick=thick,color=color
xyouts,xmid+txoff,ymid+tyoff,text,/normal,align=0.5,charsize=charsize,color=color
return
end