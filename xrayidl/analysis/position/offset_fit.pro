pro offset_fit,xobs,yobs,xopt,yopt,wvec,val
;-
; get x, y and angular offsets by fitting X-ray source positions to relatively
; well defined optical or radio positions of the sources.
; Small angular offset is assumed (i.e., val(2) << 1.)
; xobs,yobs - vectors containing ra and dec positions (arcsec) of the x-ray 
;		source positions relative to the assumed rotation center
; xopt,yopt - vectors containing the corresponding positions of the optical 
;		counterparts
; wvec - vectors containing the weights of the least square fit for 
;	the individual sources (i.e. 1/(xperr*xperr+operr*operr))
; val - vector containing the best fit parameter of x, y and angular offsets
;	(in radian)
;
; writen by wqd, May 4, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - offset_fit,xobs,yobs,xopt,yopt,wvec,val'
return
endif
ww=total(wvec)
x=total(xobs*wvec)
y=total(yobs*wvec)
xo=total(xopt*wvec)
yo=total(yopt*wvec)
xx=total(xobs*xobs*wvec)
yy=total(yobs*yobs*wvec)
yxopt=total(xopt*yobs*wvec)
xyopt=total(xobs*yopt*wvec)
a=fltarr(3,3)
;remember the IDL matrix order is different from the convetional!
a(0:2,0)=[ww,0.,y]
a(0:2,1)=[0.,ww,-x]
a(0:2,2)=[-y,x,-(xx+yy)]
b=[(xo-x),(yo-y),(yxopt-xyopt)]
val=b
;solve the linear equations
ludcmp,a,indx,d ;standard IDL routine
lubksb,a,indx,val
end
