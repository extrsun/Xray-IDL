pro gislinxy,coeff,cal,detx,dety,deltax,deltay,x,y,xl,yl,ipo,jpo
if n_params(0) eq 0 then begin
  print,'GISLINXY,coeff,cal,detx,dety,deltax,deltay,x,y,xl,yl,xlp,ylp '
  print,' LINEARIZE X and Y GIS COORDS '
  print,' Get the input calibration using GTGISMAP '
 retall
end
xyscale=detx.xyscale
nxy=n_elements(x) &print,'SIZE: ',nxy,n_elements(y)
seed1=389279 & seed2=923747 & seed3=576327 & seed4=623578
rx1=randomu(seed1,nxy)
ry1=randomu(seed2,nxy)
;make sure x and y lie in the range 0-255
x=x*(x le 255) & x=x*(x ge 0) & y=y*(y ge 0) & y=y*(y le 255)
xx=(float(x)+rx1)/xyscale & yy=(float(y)+ry1)/xyscale
rx=0.5-randomu(seed3,nxy)
ry=0.5-randomu(seed4,nxy)
tx= coeff(0) + coeff(1)*xx + coeff(2)*yy
ty= coeff(3) + coeff(4)*xx + coeff(5)*yy
sx=(tx+detx.offset)/detx.scale
sy=(ty+dety.offset)/dety.scale
ip=fix(sx) & jp=fix(sy) 
ip=ip*(ip ge 1) & ip=ip*(ip le 256) & jp=jp*(jp ge 1)&jp=jp*(jp le 256)
ipo=ip-1 & jpo=jp-1
dx=deltax(ipo,jpo)
dy=deltay(ipo,jpo)
wtx=where((tx ge 0.0),ctxp)
wtxn=where((tx lt 0.0),ctxn)
wty=where((ty ge 0.0),ctyp)
wtyn=where((ty lt 0.0),ctyn)
id=ip & jd=jp
if ctxp gt 0 then id(wtx) = ip(wtx)+1
if ctxn gt 0 then id(wtxn) = ip(wtxn)-1
if ctyp gt 0 then jd(wty) = jp(wty)+1
if ctyn gt 0 then jd(wtyn) = jp(wtyn)-1
id=id*(id gt 1)&id=id*(id le 256)&jd=jd*(jd ge 1)&jd=jd*(jd le 256)
dx2=2.*abs(deltax(id-1,jpo)-dx)*rx
dy2=2.*abs(deltay(ipo,jd-1)-dy)*ry
rrx=(tx+dx+dx2)*cal.detcos-(ty+dy+dy2)*cal.detsin
rry=(tx+dx+dx2)*cal.detsin+(ty+dy+dy2)*cal.detcos
fx=((rrx/detx.scale)+detx.center)*xyscale
fy=(dety.center-(rry/dety.scale))*xyscale
xl=fix(fx)+detx.pix1  & yl=fix(fy)+dety.pix1 
xl=xl*(xl ge 0)&xl=xl*(xl le 256)&yl=yl*(yl ge 0)&yl=yl*(yl le 256)
return
end
