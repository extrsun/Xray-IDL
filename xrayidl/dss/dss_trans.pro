pro dss_trans,xx,yy,ppo,a,b,xpixelsz,ypixelsz,kaci,eta
;-
; calculate the RA and Dec distances (in units of arcsec) of plate pixels
; from the plate center of a DSS image  using
; the algorithm described in Disk 61 map of DSS
; writen by WQD, June 5, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - dss_trans,xx,yy,ppo,a,b,xpixelsz,ypixelsz,kaci,eta
return
endif
; transfer to the pixel coordinates relative to the center of the plate
; in the direction of RA and Dec
x=(ppo(2)-xpixelsz*xx)*0.001
y=(ypixelsz*yy-ppo(5))*0.001
;
; taking care of the nonlinear transformation of the plate
xy2=x*x+y*y
x2=x*x
y2=y*y
xy=x*y
num=n_elements(xx)
val1=fltarr(num,13)
val2=fltarr(num,13)
val1(0)=[x,y,replicate(1.,num),x2,xy,y2,xy2,x2*x,x2*y,x*y2 $
	,y2*y,x*(xy2),x*(xy2)*(xy2)]
val2(0)=[y,x,replicate(1.,num),y2,xy,x2,xy2,y2*y,x*y2,y*x2 $
	,x2*x,y*(xy2),y*(xy2)*(xy2)]
kaci=val1#a(0:12)
eta=val2#b(0:12)
if n_elements(kaci) eq 1 then begin
	kaci=kaci(0)
	eta=eta(0)
endif
end
