pro ell_mub,th,qq,mub
;+
; calculate the second momentum of a 2-D elliptical surface of uniform density
;	(background)
; th - angle between  the major intrinsic axis of the ellipse and the y axis
; 	(north through east; radian)
; qq - the ratio of the minor to major axes.
; mub - output tensor containing the second momuntum (assuming a unit major
;		axis.
; written by wqd, 9/29/96
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - ell_mub,th,qq,mub'
return
endif
; coordinate rotation transform matrix:
trans=fltarr(2,2)
trans(0)=[cos(th),-sin(th),sin(th),cos(th)]

mub=fltarr(2,2)
;mub(0)=[cos(th)^2+qq^2*sin(th)^2,0. $
;	,0.,sin(th)^2+qq^2*cos(th)^2]*0.25 ; in the ellipse frame

mub(0)=[qq^2,0.,0.,1.]*0.25 ; in the ellipse frame
mub=trans##mub##transpose(trans) 
if !debug eq 1 then stop
return
end