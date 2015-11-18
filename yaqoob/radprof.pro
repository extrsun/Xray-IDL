pro radprof,nbin,fwhm,geo,plist,rcen,inten,qname=qname
if n_params(0) eq 0 then begin
  print,' radprof,nbin,fwhm,geo,plist,rcen,inten,qname=qname '
  print,' comput radial profile for a source photon list'
  retall
end
print,'selected centre ',geo.cen1
mkimage,plist,pimg
xl=min(plist.x)
yl=min(plist.y)
xfirst=geo.cen1(0)-xl+1.
yfirst=geo.cen1(1)-yl+1.
print,'estimated centre ',xfirst,yfirst
cntrd,pimg,xfirst,yfirst,x0,y0,fwhm
x0=x0+xl & y0=y0+yl
print,'found centroid ',x0,y0
del=geo.d1(0)/float(nbin)
rlo=findgen(nbin)*del & rhi=rlo+del & rcen=(rlo+rhi)/2.0
x=plist.x & y=plist.y 
;x0=geo.cen1(0) & y0=geo.cen1(1)
rad=(x-x0)*(x-x0)+(y-y0)*(y-y0)
inten=fltarr(nbin)
rhisq=rhi*rhi & rlosq=rlo*rlo
area=rhisq-rlosq
for j=0l,nbin-1 do begin
 w=where((rad gt rlosq(j) and rad le rhisq(j)),count)
 if count gt 0 then begin
 ;print,count,j,(size(rad(w)))(1),size(inten),inten(j)
  inten(j) = count/area(j)
 endif
endfor
window,1
plot,rcen,inten
oplot,rcen,inten,psym=1
if n_elements(qname) ne 0 then begin
openw,1,qname
 printf,1,'! radial profile from RADPROF '
 printf,1,'!centre: ',x0,y0
 printf,1,'LA X RADIUS FROM CENTRE (PIXELS)'
 printf,1,'LA Y Intensity in funny units '
 for k=0l,nbin-1 do begin
  printf,1,rcen(k),inten(k)
 endfor
close,1
endif
return
end
