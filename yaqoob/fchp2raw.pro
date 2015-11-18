pro fchp2raw,xin,yin,xr,yr,sistyp,ccd
if n_params(0) eq 0 then begin
 print,'fchp2raw,xin,yin,xr,yr,sistyp,ccd'
 print,' Take x,y and ccd arrays from 4 chip image and convert '
 print,' back to raw coordinates. Exact reverse of raw2new '
 print,' sistyp = 0 or 1 for S0 or S1 respectively'
 retall
end
xl=1 & xh=xl+859 & yl=1 & yh=yl+842
np=n_elements(xin) 
if np eq 1 then begin
 x=intarr(1) & y=intarr(1)
endif
x=xin & y=yin
xr=intarr(np) & yr=xr
;in raw2new the last thing done was x=x-1 and y=y-1 so
x=x+1 & y=y+1
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
 xr(wc0)= xl+424-x(wc0)
 yr(wc0)= yh+2-y(wc0)
endif
if c1 gt 0 then begin
 xr(wc1)= xh+6-x(wc1)
 yr(wc1)= yh+2-y(wc1)
endif
if c2 gt 0 then begin
 xr(wc2)= x(wc2)-xl-435
 yr(wc2)= y(wc2)-yl+2
endif
if c3 gt 0 then begin
 xr(wc3)= x(wc3)-xl+6
 yr(wc3)= y(wc3)-yl+2
endif
if np eq 1 then begin
 xr=xr(0) & yr=yr(0)
endif
return
end
