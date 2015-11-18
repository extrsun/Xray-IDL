pro ann,list,t,a1=a1,a2=a2,ad=ad

sz=size(t)
dimx=sz(1)
dimy=sz(2)
if n_elements(a1) eq 0 then a1=0
if n_elements(a2) eq 0 then a2=360.
if n_elements(ad) eq 0 then ad=1.
dist_circle,dis,dimx,cpx,cpy
sel=where(t gt 0. and dis ge r1 and dis le r2,nsel)
c=lindgen(dimx*dimy)
x=list.x & y=list.y
dis=sqrt((x-cpx)*(x-cpx)+(y-cpy)*(y-cpy))
sa=(y-cpy)/dis 
ca=(x-cpx)/dis 
a=asin(sa)*180./!pi
if ca lt 0 then a=180-a
a=a+90
print,minmax(a)
flux=histogram(a,min=a1,max=a2,binsize=ad)
angle=fix(min(a))+(n_elements(flux)+0.5)*ad

end