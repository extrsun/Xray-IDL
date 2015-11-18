pro gislinexp,coeff,detx,dety,deltax,deltay,x,y,xl,yl,xlp,ylp,tx,ty,dx,dy
if n_params(0) eq 0 then begin
  print,'GISLINXY, coeff, detx, dety, deltax, deltay, x, y, xl, yl, xlp, ylp '
  print,' LINEARIZE X and Y GIS COORDS '
  print,' Get the input calibration using GTGISMAP '
 retall
end
;make sure x and y lie in the range 1-256
;x=x<256 & x=x>1 & y=y>1 & y=y<256
xx=float(x) & yy=float(y)
nxy=n_elements(x) &print,'SIZE: ',nxy,n_elements(y)
dx=deltax(x-1,y-1)
dy=deltay(x-1,y-1)
seed1=39489323
seed2=89094734
rx=0.5-randomu(seed1,nxy)
ry=0.5-randomu(seed2,nxy)
window,1
plot,histogram(rx,binsize=0.05)
window,2
plot,histogram(ry,binsize=0.05)
print,minmax(rx)
print,' offset x & y',detx.offset,detx.offset
print,' scale x & y',detx.scale,dety.scale
tx= coeff(0) + coeff(1)*xx + coeff(2)*yy
ty= coeff(3) + coeff(4)*xx + coeff(5)*yy
print,' tx ',minmax(tx)
print,' dx ',minmax(dx)
fx=(tx + dx + rx +detx.offset)/detx.scale
fy=(ty + dy + ry +dety.offset)/dety.scale
xs=(tx + detx.offset)/detx.scale
ys=(ty + dety.offset)/dety.scale
;fx=fx>0. & fx=fx<257. & fy=fy>0. & fy=fy<257.
xl=fix(fx)  & yl=257-fix(fy) 
xlp=fix(xs) & ylp=fix(ys) & xlp=xlp>1 & xlp=xlp<256 & ylp=ylp>1 & ylp=ylp<256
return
end
