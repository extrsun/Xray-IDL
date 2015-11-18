pro raw2new,xrin,yrin,x,y,sistyp,ccd
if n_params(0) eq 0 then begin
 print,'raw2new,xrin,yrin,x,y,sistyp,ccd'
 print,'convert rawx and rawy coords to 4 chip image'
 retall
end
np=n_elements(xrin) 
if np eq 1 then begin
 xr=intarr(1) & yr=xr
endif
xr=xrin & yr=yrin
x=intarr(np) &y=x
;xl=209 & xh=xl+859 & yl=209 & yh=yl+842
xl=1 & xh=xl+859 & yl=1 & yh=yl+842
xr=xr*(xr ge 7) & xr=xr*(xr le 424) & yr=yr*(yr ge 2) & yr=yr*(yr le 421)
if sistyp eq 0 then begin
wc0=where((ccd eq 0),c0)
wc1=where((ccd eq 1),c1)
wc2=where((ccd eq 2),c2)
wc3=where((ccd eq 3),c3)
endif
if sistyp eq 1 then begin
wc0=where((ccd eq 2),c0)
wc1=where((ccd eq 3),c1)
wc2=where((ccd eq 0),c2)
wc3=where((ccd eq 1),c3)
endif
if c0 gt 0 then begin
 x(wc0) = xl+424-xr(wc0)
 y(wc0) = yh+2-yr(wc0)
endif
if c1 gt 0 then begin
 x(wc1) = xh+6-xr(wc1)
 y(wc1) = yh+2-yr(wc1)
endif
if c2 gt 0 then begin
 x(wc2) = 435+xl+xr(wc2)
 y(wc2) = yl-2+yr(wc2)
endif
if c3 gt 0 then begin
 x(wc3) = xl+xr(wc3)-6
 y(wc3) = yl+yr(wc3)-2
endif 
x=x-1 & y=y-1
x=x*(x ge 0) & x=x*(x le 863)&y=y*(y ge 0)&y=y*(y le 847)
if np eq 1 then begin
x=x(0) & y=y(0)
endif
return
end
