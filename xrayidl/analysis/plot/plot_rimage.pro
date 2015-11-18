pro plot_rimage,image,cor
;-
; get an intensity image with number of randum points per pixel proportional
; to the intensity
; image - the input intensity image (the intensity should be scaled properly)
; nx1,nx2,ny1,ny2 - the normalized corner positions
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - plot_rimage,image,nx1,nx2,ny1,ny2'
return
endif

im=image
sz=size(im)
xpixn=(cor(1)-cor(0))/sz(1)
ypixn=(cor(3)-cor(2))/sz(1)

loc=lindgen(sz(1),sz(2))
i=float(loc mod sz(1))
j=float(loc/sz(1))
sel=where(im lt 1. and im gt 0., nsel) 
if nsel ne 0 then begin 
	poisson,im(sel),ran,seed
	im(sel)=ran
endif
im=nint(im)
c=where(im gt 0,nc)
tc=total(im(c))
x=fltarr(tc)
y=fltarr(tc)
;
kk=0L
for k=0L,nc-1L do begin
	loc=c(k)
	nk=im(loc)
	x(kk:kk+nk-1)=i(loc)+randomu(seed,nk)
	y(kk:kk+nk-1)=j(loc)+randomu(seed,nk)
	kk=kk+nk
endfor
x=cor(0)+x*xpixn
y=cor(2)+y*ypixn
plots,x,y,psym=3,/normal

end