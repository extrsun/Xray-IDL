pro mkstatpapfig2,fekfiga,fekfigb,gam 
openw,11,'statpapfig2.qdp'
printf,11,'SKIP SINGLE'
printf,11,'PLOT VERTICAL'
printf,11,'@statpapfig2.pco'
setmod01_1a,pfix,pnames,statnames,parfnames,outname
xstats,pfix,pnames,statnames,parfnames,outname=outname,strue,smin,bfits,dels
fekfiga=bfits(0:999,3:3) & gam=bfits(0:999,1:1)
setmod01_1b,pfix,pnames,statnames,parfnames,outname
xstats,pfix,pnames,statnames,parfnames,outname=outname,strue,smin,bfits,dels
fekfigb=bfits(0:999,3:3)
print,' (a) MIN MAX ',minmax(fekfiga)
print,' (b) MIN MAX ',minmax(fekfigb)
print,' (c) MIN MAX ',minmax(gam)
mkhist,fekfiga,1000,loc1,xval,yval,xp,yp
fekapk=xval(where(yval eq max(yval)))
print,'Peak of a distribution ',fekapk
mkhist,fekfiga,1000,loc1,xval,yval,xp,yp
yp=100.*yp/yp(0)
;fekafit=gaussfit(xval,yval,coeff1)
;print,'Gaussian fit to (a):'
;print,coeff1
;plot,xp,yp & oplot,xval,fekafit
np=(size(xp))(1)
ng=(size(xval))(1)
for k=0l,np-1 do printf,11,xp(k),yp(k) 
printf,11,'NO 	NO'
mkhist,fekfigb,1000,loc1,xval,yval,xp,yp
fekbpk=xval(where(yval eq max(yval)))
print,'Peak of b distribution ',fekbpk
mkhist,fekfigb,250,loc1,xval,yval,xp,yp
plot,xp,yp
np=(size(xp))(1)
ng=(size(xval))(1)
for k=0l,np-1 do printf,11,xp(k),yp(k)
printf,11,'NO    NO'
mkhist,gam,1000,loc1,xval,yval,xp,yp
gampk=xval(where(yval eq max(yval)))
print,'Peak of c distribution ',gampk
mkhist,gam,250,loc1,xval,yval,xp,yp
plot,xp,yp
np=(size(xp))(1)
ng=(size(xval))(1)
for k=0l,np-1 do printf,11,xp(k),yp(k)
printf,11,'NO    NO'
close,11
return
end
