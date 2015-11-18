pro ell_plot,para,rr,corner,hdr,xc,yc,lines=lines,color=color
;+
; NAME:
;	ELL_PLOT
;
; PURPOSE:
;       Plot an ellipse 
;
; CALLING SEQUENCE:
;     ELL_PLOT,para,rr,corner,hdr,xc,yc,lines=lines,color=color
;     This routine is now replaced by PLOT_SHAPE
;
; INPUTS:
; para - ellipse parameters from ell_it (in pixels)
; rr - semimajor axis (arcmin)
; cor - plot position from cont_grey
; hdr - fits header
; xc , yc - offsets  (in pixels) of the pixel ref center relative to
;           the image ref center
; lines, color - curve style and color
;
; written by wqd, 9/29/96
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - ell_plot,para,rr,corner,hdr,xc,yc'
print,',lines=lines,color=color'
return
endif
if n_elements(xc) eq 0 then xc=0
if n_elements(yc) eq 0 then yc=0
; in the ellipical coordinates
ga=findgen(1000)/(2.*!pi)
al=para(3) ;-0.5*!pi
xxe=(float(rr)*60.)*cos(ga)
yye=(1.-para(2))*(float(rr)*60)*sin(ga)

; get to the plot coordinates
xs=xxe*cos(al)-yye*sin(al)+(para(0)-xc)*!size_pixel
ys=xxe*sin(al)+yye*cos(al)+(para(1)-yc)*!size_pixel
plot_add,hdr,corner,xs,ys,lines=lines,color=color
return
end

