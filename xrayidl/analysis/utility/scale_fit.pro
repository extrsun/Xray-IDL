pro scale_fit,r,xobs,yobs,xopt,yopt,wvec,val,index=index
;-
; get the image scale correction as a function of val(0)+val(1)*offaxiz^index
; r - offaxix radius in units of arcmin
; index - default = 2
; xobs,yobs - vectors containing ra and dec positions (arcsec) of the x-ray 
;		source positions relative to the assumed rotation center
; xopt,yopt - vectors containing the corresponding positions of the optical 
;		counterparts
; wvec - vectors containing the weights of the least square fit for 
;	the individual sources (i.e. 1/(xperr*xperr+operr*operr))
; val - vector containing the best fit parameters
;
; writen by wqd, Jan 2, 1995
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - scale_fit,r,xobs,yobs,xopt,yopt,wvec,val,index=index'
return
endif
if n_elements(index) eq 0 then index=2
ww=total(wvec)

xobswv=xobs^2*wvec
yobswv=yobs^2*wvec
xx=total(xobswv)
yy=total(yobswv)
xxr=total(xobswv*r^index)
yyr=total(yobswv*r^index)

xxr2=total(xobswv*r^(2*index))
yyr2=total(yobswv*r^(2*index))

xxopt=total(xopt*xobs*wvec)
yyopt=total(yopt*yobs*wvec)

xxoptr=total(xopt*xobs*wvec*r^index)
yyoptr=total(yopt*yobs*wvec*r^index)

a=fltarr(2,2)
;remember the IDL matrix order is different from the convetional!
a(0:1,0)=[xx+yy,xxr+yyr]
a(0:1,1)=[xxr+yyr,xxr2+yyr2]
b=[xxopt-xx+yyopt-yy,xxoptr-xxr+yyoptr-yyr]
val=b
;solve the linear equations
ludcmp,a,indx,d ;standard IDL routine
lubksb,a,indx,val
end
