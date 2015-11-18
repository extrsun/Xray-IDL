pro plot_ell,el,pa,rr,corner,hdr,gradec,lines=lines,color=color,deg=deg,sel=sel,fill=fill,thick=thick
;+
; now replaced by plot_shape.pro
; plot an ellipse in an exiting plot.
; 
; el, pa, rr - ellipticity (1-minor/major), position angle (radian or
;              deg if keyword deg is set, and half major axis length (arcmin)
; corner - plot position from cont_grey
; hdr - fits header of the image
; gradec - the ra and dec of the ellipse if not coinciding with the
;          image center (deg)
; lines, thick, color - curve style, thickness, and color 
; sel - selected image pixel index within the ellipse
; fill - if set, filling of the ellipse will be performed
;
; written by wqd, 5/22/03
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - plot_ell,el,pa,rr,corner,hdr,gradec'
print,',lines=lines,color=color,deg=deg,sel=sel,fill=fill,thick=thick'
return
endif
; in the ellipical coordinates
ga=findgen(1001)*(2*!pi/1000.)
xxe=(float(rr)*60.)*cos(ga)
yye=(1.-el)*(float(rr)*60)*sin(ga)
if keyword_set(deg) then rpa=pa*!pi/180. else rpa=pa
; get to the plot coordinates
xs=xxe*sin(rpa)+yye*cos(rpa)
ys=-xxe*cos(rpa)+yye*sin(rpa)
if n_elements(gradec) ne 0 then begin
    crval=sxpar(hdr,'crval*',count=nc)
    if nc gt 2 then print,'more than 2 crval parameters in the header!!'
    trans_dist,crval(0),crval(1),gradec(0),gradec(1),xp,yp,/das,/deg
    xs=xs+xp
    ys=ys+yp
endif 
;plot_sel,hdr,corner,xs,ys,sel,lines=lines,color=color,fill=fill,thick=thick
plot_add,hdr,corner,xs,ys,lines=lines,color=color,fill=fill,thick=thick,sel=sel
return
end

