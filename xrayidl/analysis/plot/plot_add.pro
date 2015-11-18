pro plot_add,hdr,corner,xs,ys,linestyle=linestyle,color=color,fill=fill,thick=thick,sel=sel,noplot=noplot,nsign=nsign,ecolor=ecolor
;+
; add a curve in an existing sky plot
; hdr - fits header
; corner - corner positions of the plot
; xs, ys - curve coordinates in arcsec (relative to the reference
;          pixel)
; xsn, ysn - curve coordinates in normalized coordinates of the image
; linestyle, thick, color - curve style, thickness, and color 
; sel - output selected pixels included by the curve
; nsign - property of encircled curve region. if set and equal to 1, means excluded
; ecolor - color used to draw exclusive lines (default: 255)
;
; written by wqd, 10/4/96
; add sel keyword, wqd, June 15, 2003
; add nsign keyword, sw, March 25, 2011
; add ecolor keyword, sw, March 07, 2013
; modified by sw on the coordinate calculation, Sep. 11, 2013
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - plot_add,hdr,corner,xs,ys,linestyle=linestyle,color=color,fill=fill,thick=thick,sel=sel,noplot=noplot,nsign=nsign'
return 
endif
if N_elements(linestyle) eq 0 then linestyle=0
if N_elements(thick) eq 0 then thick=1
if N_elements(nsign) eq 0 then nsign=0
if N_elements(color) eq 0 then begin
	if !d.name eq 'x' then color=!d.n_colors-1 else color=0
endif
naxis=sxpar(hdr,'naxis*')
xdims=naxis(0)*abs(sxpar(hdr,'cdelt1'))*3600.
; ydims=naxis(1)*abs(sxpar(hdr,'cdelt2'))*3600.
; xdims=naxis(0)*abs(sxpar(hdr,'cdelt1'))*3600.
ydims=naxis(1)*sxpar(hdr,'cdelt2')*3600.
xmid=corner(0)+(corner(1)-corner(0))*sxpar(hdr,'crpix1')/float(naxis(0))
ymid=corner(2)+(corner(3)-corner(2))*sxpar(hdr,'crpix2')/float(naxis(1))
xnorm=(corner(1)-corner(0))/(xdims);*cos(sxpar(hdr,'crval2')*!dpi/180.)
ynorm=(corner(3)-corner(2))/(ydims)
xsn=xmid+xs*xnorm
ysn=ymid+ys*ynorm
; The complicate calculation is used to transfered the distance (in 
; arcsec) to the reference points (crval*) into the relative positions

c=where(xsn ge corner(0) and xsn le corner(1) and $
            ysn ge corner(2) and ysn le corner(3),nc)
    if nc ne 0 then begin
     if keyword_set(noplot) eq 0 then begin
       plots,xsn,ysn,/normal,color=color $
              ,linestyle=linestyle,thick=thick $
	      ,clip=[corner(0),corner(2),corner(1),corner(3)],noclip=0
       if nsign then begin
         if keyword_set(ecolor) eq 0 then ecolor=255
         plots,[xsn(0),xsn(n_elements(xsn)/2)] $
              ,[ysn(0),ysn(n_elements(ysn)/2)] $
                ,/normal,color=ecolor,thick=thick   $
	        ,clip=[corner(0),corner(2),corner(1),corner(3)],noclip=0
       endif
     endif
    endif ;
;        plots,xsn,ysn,/normal,color=color,lines=linestyle,thick=thick $
;	,clip=[corner(0),corner(2),corner(1),corner(3)],noclip=0
if keyword_set(fill) then begin
    device, get_graphics_function=oldGrphFunc ; Remember what to restore it to
    device, set_graphics_function=6           ; Set XOR mode
    nc1 = !d.table_size-1
    polyfill,xsn*!d.x_size,ysn*!d.y_size,/dev,/noclip
    device,set_graphics=3   ;Re-enable normal copy write
    device, set_graphics_function=oldGrphFunc ; Restore old grphx mode
endif 
;sel the pixels included
xsn=(xsn-corner(0))/(corner(1)-corner(0))*naxis(0);now in the image coordinates
ysn=(ysn-corner(2))/(corner(3)-corner(2))*naxis(1)
if nc ne 0 then sel= polyfillv(xsn,ysn,naxis(0),naxis(1)) else sel=-1
if !debug eq 1 then stop,'at the end of plot_add'
return
end
