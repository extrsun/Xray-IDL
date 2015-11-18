pro eefdata,bgd,bkgd,inst,plist,xcen,ycen,npts,rmin,rmax,rnorm,r0,peef,peeferr,$
theta0,dir=dir,eefname=eefname,qdpname=qdpname,xrteef,ctsnorm
if n_params(0) eq 0 then begin
print,'eefdata,bgd,bkgd,inst,plist,xcen,ycen,npts,rmin,rmax,rnorm,r0,peef,'
print,'peeferr,theta0,dir=dir,eefname=eefname,qdpname=qdpname,xrteef,ctsnorm'
 print,'Calculate EEF from a photon list. Must have detector coords'
 print,'If DIR is specified, compare with expected XRT EEF '
 print,'** INPUTS ** '
 print,'BGD 	- constant = background count rate per sq mm '
 print,'BKGD 	- constant = background count rate per sq mm '
 print,'	- BGD is subtracted from data, BKGD is worked into '
 print,'	- predicted EEF. Either BGD or BKGD MUST be zero. '
 print,'PLIST	- photon list '
 print,'INST 	- 0..3 for S0..S3'
 print,'XCEN, YCEN - centroid of source in DET coords '
 print,'NPTS 	- Number of radii '
 print,'RMIN RMAX - Bounds of radii (mm) '
 print,'RNORM	- Normalize the EEF at this radius (mm) '
 print,'THETA0  - Angle in degress from optcial axis '
 print,'DIR	- directory containing XRT EEF file '
 print,'EEFNAME - Name of XRT EEF file '
 print,'QDPNAME - Rootnames of output QDP files with EEF and RAD prof '
 print,'** OUPUTS ** '
 print,'R0	- output radii (mm) '
 print,'PEEF	- EEF array to go with r0'
 print,'PEEFERR - Error on EEF '
 retall
endif
if n_elements(qdpname) le 0 then begin
 qdpname=' '
 read,'Enter output QDP root filename ',qdpname
endif
openw,1,qdpname+'.eef'
openw,2,qdpname+'.rpf'
printf,1,'READ SERR 1 2 '
printf,1,'SKIP SINGLE'
printf,1,'MARKER 17 on 1 '
printf,1,'LINE ON 2..5'
printf,1,'LTYPE 2 ON 4 5'
printf,1,'la x r (mm) '
printf,1,'la y EEF (normalized at ',rnorm,' mm)'
printf,1,'log y on '
printf,2,'READ SERR 1 2'
printf,2,'SKIP SINGLE'
printf,2,'MARKER 17 on 1'
printf,2,'LINE ON 2..5'
printf,2,'LTYPE 2 on 4 5'
printf,2,'la x r (mm) '
printf,2,'la y Normalized cts mm\u-2\d '
printf,2,'log y on'
if inst le 1 then mmscal=0.027 else mmscal=0.25
;calculate radii for equal areas
rmaxsq=rmax*rmax & rminsq=rmin*rmin
dela = (rmaxsq-rminsq)/float(npts-1)
;rsq = rminsq+findgen(npts)*dela & rad=sqrt(rsq)
delr=(rmax-rmin)/float(npts-1) & rwid=delr/2.
dummy=fltarr(npts)
rad=rmin+findgen(npts)*delr
rhi=rad & rlo=dummy & rlo(0)=0. & rlo(1:npts-1)=rad(0:npts-2)
rcen=(rlo+rhi)/2. & rerr=(rhi-rlo)/2.
radprof=dummy & proferr=dummy & area=dummy & predprof=dummy
r0=rad
pi=3.14159265359
cts=fltarr(npts) & ctserr=cts
xp=plist.detx & yp=plist.dety 
rp = sqrt((xp-xcen)*(xp-xcen)+(yp-ycen)*(yp-ycen))*mmscal
ncts=fltarr(npts)
for k=0l,npts-1 do begin
 wr=where((rp le rad(k)),nwr)
 ncts(k)=float(nwr)
 bgdcts=3.14159*rad(k)*rad(k)*bgd
 cts(k)=float(nwr) -bgdcts 
 ctserr(k) = sqrt(float(nwr)+bgdcts)
 if k eq 0 then begin
  area(0)=rad(0)*rad(0)*pi
  radprof(0)=cts(0)/area(0)
  proferr(0)=ctserr(0)/area(0)
  printf,2,'!',rcen(0),rerr(0),radprof(0),proferr(0)
  printf,2,rcen(0),rerr(0),1.0,proferr(0)/radprof(0)
 endif
 if k gt 0 then begin
  area(k)=pi*(rad(k)*rad(k)-rad(k-1)*rad(k-1)) 
  radprof(k)=(cts(k)-cts(k-1))/area(k)
  proferr(k)=sqrt(ncts(k)-ncts(k-1)+area(k)*bgd)/area(k)
  nradprof=radprof(k)/radprof(0)
  nproferr=nradprof*((proferr(0)/radprof(0))+(proferr(k)/radprof(k)))
  printf,2,rcen(k),rerr(k),radprof(k)/radprof(0),proferr(k)/radprof(0)
 endif
endfor
printf,2,'no no no no'
;find norm factor
xout=fltarr(1) & yout=fltarr(1)
xout(0)=rnorm 
fint,rad,cts,xout,yout
ctsnorm=yout(0)
peef=cts/ctsnorm
peeferr=ctserr/ctsnorm
for k=0l,npts-1 do printf,1,r0(k),rwid,peef(k),peeferr(k)
printf,1,'no no no no'
if n_elements(dir) gt 0 then begin
; find EEF at rnorm
 ascaeef,theta0,rnorm,dir=dir,eefname=eefname,outeef,normeef
 ascaeef,theta0,r0,dir=dir,eefname=eefname,outeef,xrteef 
 for m=0l,3 do begin 
 xrteef(0:npts-1,m)=xrteef(0:npts-1,m)/normeef(m)
 xrteef(*,m)=xrteef(*,m)*$
 (1.-(pi*rnorm*rnorm*bkgd/ctsnorm))+(pi*r0*r0*bkgd/ctsnorm)
 for k=0l,npts-1 do begin
	printf,1,r0(k),dummy(k),xrteef(k,m),dummy(k)
  if k eq 0 then begin
        predprof(0)=xrteef(0,m)/area(0)
	printf,2,'!',rcen(0),dummy(0),predprof(k),dummy(k)
	printf,2,rcen(0),dummy(0),1.0,dummy(k)
  endif
  if k gt 0 then begin
	predprof(k)=(xrteef(k,m)-xrteef(k-1,m))/area(k)/predprof(0)
	printf,2,rcen(k),dummy(k),predprof(k),dummy(k)
  endif
 endfor
 printf,1,'no no no no'
 printf,2,'no no no no'
endfor
endif
close,1
close,2
return
end
