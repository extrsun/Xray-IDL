pro combcont,fname1,fname2,nstep1,nstep2,x1,x2,y1,y2,dx1,dx2,dy1,dy2,lev1,lev2,x1cen,y1cen,x2cen,y2cen,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax
if n_params(0) eq 0 then begin
 print,'combcont,fname1,fname2,nstep1,nstep2,x1,x2,y1,y2,dx1,dx2,dy1,dy2,lev1,lev2,x1cen,y1cen,x2cen,y2cen,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax'
 retall
end
ns1=nstep1+1 & ns2=nstep2+1
sis = datxfer(fname1,ns1,0)
nx1 = (size(sis))(2) 
tsis = transpose(sis)
gis = datxfer(fname2,ns2,0)
nx2 = (size(gis))(2)
tgis = transpose(gis)
x1u = x1+float(nx1)*dx1 & x2u=x2+float(nx2)*dx2
y1u=y1+float(ns1)*dy1 & y2u=y2+float(ns2)*dy2
x1a=findgen(nx1)*dx1+x1
y1a=findgen(ns1)*dy1+y1
x2a=findgen(nx2)*dx2+x2
y2a=findgen(ns2)*dy2+y2
if n_elements(xmin) eq 0 then xmin=0.95*min([x1,x2])
if n_elements(xmax) eq 0 then xmax=1.05*max([x1u,x2u])
if n_elements(ymin) eq 0 then ymin=0.95*min([y1,y2])
if n_elements(ymax) eq 0 then ymax=1.05*max([y1u,y2u])
;contour,sis,levels=lev1,$
contour,tsis,levels=lev1,$
x1a,y1a,xr=[xmin,xmax],yr=[ymin,ymax],/xs,/ys
contour,tgis,levels=lev2,$
x2a,y2a,xr=[xmin,xmax],yr=[ymin,ymax],/over,$
c_linest=1
oplot,[x1cen],[y1cen],psym=1
oplot,[x2cen],[y2cen],psym=1
return
end
