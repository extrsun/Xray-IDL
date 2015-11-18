pro pspcprof,plist,pilo,pihi,xcen,ycen,bgd,nrad,rmin,rmax,rcen,rwid,cts,ctserr,psf,matrix=matrix,nocent=nocent,qdpname=qdpname,spec
if n_params(0) eq 0 then begin
 print,'Compute radial profile for a PSPC photon list and compare it '
 print,'with the PSF.'
 print,'pspcprof,plist,pilo,pihi,xcen,ycen,bgd,nrad,rmin,rmax,rcen,rwid,cts,ctserr,psf,matrix=matrix,nocent=nocent,qdpname=qdpname,spec'
 print,'**INPUTS** '
 print,'PLIST   - PSPC photon list '
 print,'PILO,PIHI - Min and Max PI channels to use '
 print,'BGD 	  - Average Background cts/pixel [=abs(bgd)]'
 print,'	  - Note -ve value of BGD uses abs(bgd) to compute the'
 print,'	  - correct norm for predicted PSF but does NOT include'
 print,'	  - bgd in the predicted PSF '
 print,'XCEN, YCEN - centre, use dummy values of NOCENT<=0 '
 print,'NRAD    - Number of radial bins '
 print,'RMIN, RMAX - minimum and maximum radii : arcmin '
 print,'	   - Note first radial bin will have bounds 0-RMIN '
 print,'MATRIX 	   - PSPC response matrix to get energy bounds from '
 print,'NOCENT	   - >0 if you don t want to compute centroid '
 print,'QDPNAME    - Name of QDP file to write results to '
 print,'**OUTPUTS** '
 print,'RCEN	- Radial bin centres (arcmin) '
 print,'RWID    - Radial bin half-widths '
 print,'CTS	- Cts per sq arcmin '
 print,'CTSERR  - Error on above '
 print,'PSF 	- Predicted PSF '
 retall
end
if n_elements(qdpname) eq 0 then begin
 qdpname=' '
 read,'Enter name of QDP file to write results to ',qdpname
endif
;for the moment assume the source is On-axis -
;will eventually compute theta properly
theta=0.0
;get energy bounds
tab=readfits(matrix,h,ext=2)
str0=h(9)
str1=h(11) & str2=h(14)
corr='1E' & cor0='1I' & strput,str0,cor0,11
strput,str1,corr,11 & strput,str2,corr,11
h(9)=str0 & h(11)=str1 & h(14)=str2
;chans=tbget(h,tab,'channel')
elo=tbget(h,tab,'e_min')
ehi=tbget(h,tab,'e_max')
ecen=0.5*(ehi+elo) 
cts=fltarr(nrad) & ctserr=cts & rcen=fltarr(nrad)
rl=fltarr(nrad) & rh=rl & area=rl & psf=fltarr(nrad) 
qlist=plist(where(plist.pi ge pilo and plist.pi le pihi))
x=qlist.x & y = qlist.y
if nocent le 0 then begin
 xran=minmax(x)-1.0 & yran=minmax(y)-1.0
 xdim = min([512,(xran(1)-xran(0)+1)])
 ydim = min([512,(yran(1)-yran(0)+1)])
 dims=[xdim,ydim]
 XY2IMAGE,x,y,qlist.pha,xran,yran,dims,xbns,ybns,image,pimage
 centroid,image,xc,yc
 xcen=xbns(fix(xc),2) & ycen=ybns(fix(yc),2)
endif
print,'Using centre ',xcen,ycen
;set up radial bins
delr=(rmax-rmin)/float(nrad-1)
rh=rmin+findgen(nrad)*delr
rl(1:nrad-1)=rh(1:nrad-1)-delr & rl(0)=0.0
rwid=0.5*(rh-rl) & rcen=0.5*(rh+rl)
;scale = armin per pixel
scale = 0.468750/60.
rlsq=rl*rl & rhsq=rh*rh
rsq=(x-xcen)*(x-xcen)+(y-ycen)*(y-ycen) & rsq=rsq*scale*scale
;must reject data outside rmax
rmaxsq=rmax*rmax
rsq = rsq(where(rsq le rmaxsq))
qlist=qlist(where(rsq le rmaxsq))
if rmaxsq gt 1.01*max(rsq) then begin
 print,' **WARNING** Your RMAX is greater than the region covered '
 print,'             by the data so your norms may off '
endif 
;make pspc spectrum
spec=histogram([0,qlist.pi],max=255,binsize=1)
;now fill the radial bins
avpi=(pilo+pihi)/2 & dumspec=fltarr(1)+1.
for k=0l,nrad-1 do begin
 wr=where((rsq ge rlsq(k) and rsq lt rhsq(k)),nwr)
 area(k) = !pi*(rhsq(k)-rlsq(k))
 cts(k)=float(nwr)/area(k)
 ctserr(k)=sqrt(float(nwr))/area(k)
 psf(k) = pspcoff(rcen(k),theta,ecen(pilo-1:pihi-1),spec(pilo-1:pihi-1))
endfor
;normaliztion for psf
;integrate the theoretical psf between 0.0 and rmax
nb=100
delr=rmax/float(nb+1)
rb=findgen(nb+1)*delr
rc=0.5*(rb(1:nb)+rb(0:nb-1)) & rw=rb(1:nb)-rb(0:nb-1)
ppsf=fltarr(nb)
for i=0,nb-1 do ppsf(i)=pspcoff(rc(i),theta,ecen(pilo-1:pihi-1),spec(pilo-1:pihi-1))
intpsf = total(ppsf*2.*rc*rw*!pi)
;this integral should be equal to the source counts enclosed
;within rmax.
print,'Integral of PSF between 0 and RMAX = ',intpsf
print,'Total Source + Bgd counts in region = ',total(spec(pilo-1:pihi-1))
bgdcts= !pi*rmaxsq*abs(bgd)/scale/scale
print,'Total Estimated Bgd counts in region = ',bgdcts
pnorm = (total(spec(pilo-1:pihi-1))-bgdcts)/intpsf
print,'PSF Normalization = ',pnorm
openw,1,qdpname
printf,1,'READ SERR 1 2 '
printf,1,'SKIP SINGLE '
printf,1,'MARKER 17 on 1 '
printf,1,'LA X Radius (arcmin) '
printf,1,'LA Y cts/ (arcmin)\u2\d [PI channels ',pilo,' - ',pihi,']'
printf,1,'LA OT PSPC Radial Profile - Off-axis angle = ',theta 
printf,1,'LA T BGD = ',abs(bgd)/scale/scale,' cts / (arcmin)\u2\d '
printf,1,'LOG Y '
printf,1,'LINE ON 2'
for j=0l,nrad-1 do begin
 printf,1,rcen(j),rwid(j),cts(j),ctserr(j)
endfor
printf,1,'NO NO NO NO '
dumerr=0.0
if bgd gt 0 then bgdrate=abs(bgd)/scale/scale else bgdon=0.0
for j=0l,nrad-1 do begin
 printf,1,rcen(j),dumerr,(pnorm*psf(j)+bgdrate),dumerr
endfor
close,1
return
end
