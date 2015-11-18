pro eefarea,theta0,r1,r2,eefrat,arat,dir=dir,eefname=eefname
if n_params(0) eq 0 then begin
 print,'eefarea,theta0,r1,r2,eefrat,arat,dir=dir,eefname=eefname'
 print,'For a given area of radius r1 and for a range in annulii '
 print,'of radius r2 (centred on the same point) compute the ratio '
 print,'of expected counts in the two areas for a point source and '
 print,'and compare this to the ratio of areas '
 retall
end
npts=100 & rmax=12.00 & delr=(rmax-r1)/float(npts-1)
r2=r1+findgen(npts)*delr
arat=fltarr(npts) & eefrat=arat
ascaeef,theta0,r1,dir=dir,eefname=eefname,outeef,eefr1 
ascaeef,theta0,r2,dir=dir,eefname=eefname,outeef,eefr2
eefrat=(eefr2/eefr1(0))-1.0
arat=(r2/r1(0))*(r2/r1(0))-1.0
plot,r2,eefrat
oplot,r2,arat
return
end
