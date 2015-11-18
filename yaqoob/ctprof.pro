pro ctprof,plist,inst,pi,xcen,ycen,nrad,rmin,rmax,rlo,rhi,rtr,chlo,chhi,cts
if n_params(0) eq 0 then begin
 print,'ctprof,plist,inst,xcen,ycen,rmin,rmax,rlo,rhi,rtr,chlo,chhi,cts'
 print,' Take an ASCA photon list and compute the absolute number '
 print,' of counts in nrad radial bins in 10 energy bands '
 print,' The outputs are written to a file OUTFILE and this file '
 print,' is designed to be used by subsequent programs where these '
 print,' count-rate profiles may be rebinned and normalized however '
 print,' you wish. The energy ranges for SIS are (in keV) '
 print,' 0.4-1,1-2,2-3,3-4,4-5,5-6,6-7,7-8,8-9,9-10 '
 print,' Same for GIS except that the first range is 0.6-1.0'
 print,'**INPUTS** '
 print,'PLIST 	- ASCA photon list, must carry DET coords '
 print,'INST	- = 0,1,2,3 for S0,S1,S2,S3 '
 print,'PI	- = 0 for PHA, 1 for PI ' 
 print,'XCEN, YCEN - centre, find beforehand however you wish '
 print,'NRAD	- Number of radial bins '
 print,'RMIN, RMAX - minimum and maximum radii ' 
 print,'**OUTPUTS** '
 print,'RLO	- Radial bin lower bounds (mm) '
 print,'RHI 	- Radial bin upper bounds (mm) '
 print,'RTR	- Minimum radius to chip boundary '
 print,'CTS(NRAD,10)	- Number of counts in radial bin ' 
 retall
end
npha=10
if inst le 1 then mmscal=0.027 else mmscal=0.25
cts=fltarr(nrad,npha)
;set up the energy boundaries
chlo=fltarr(4,npha) & chhi=chlo
chlo(0,0:npha-1)=[100,285,565,845,1075,1215,1355,1495,1585,1655]  
chhi(0,0:npha-1)=[284,564,844,1074,1214,1354,1494,1584,1654,1725]
chlo(1,0:npha-1)=[124,308,620,925,1130,1285,1438,1563,1642,1718]
chhi(1,0:npha-1)=[307,619,924,1129,1284,1437,1562,1641,1717,1795]
chlo(2,0:npha-1)=[50,86,171,255,340,425,510,595,680,764]
chhi(2,0:npha-1)=[85,170,254,339,424,509,594,679,763,850]
chlo(3,0:npha-1)=chlo(2,0:npha-1) & chhi(2,0:npha-1)=chhi(2,0:npha-1)
;first set up the radial bins 
delr=(rmax-rmin)/float(nrad-1)
rlo=fltarr(nrad) & rhi=rlo
rlo(0)=0.0 & rlo(1:nrad-1)=rmin+findgen(nrad-1)*delr
rhi(0)=rmin & rhi(1:nrad-1)=rlo(1:nrad-1)+delr
rcen=0.5*(rlo+rhi) & rwid=(rhi-rlo)
x=plist.detx & y=plist.dety
;compute rtr
r1=abs(xcen-max(x))
r2=abs(xcen-min(x))
r3=abs(ycen-max(y))
r4=abs(ycen-min(y))
rtr=min([r1,r2,r3,r4]) & rtr=rtr*mmscal
if pi eq 0 the ph = plist.pha esle ph=plist.pi
pradsq=(x-xcen)*(x-xcen)+(y-ycen)*(y-ycen)
pradsq=pradsq*mmscal*mmscal
rlosq=rlo*rlo & rhisq=rhi*rhi
for j=0l,nrad-1 do begin 
 for k=0,npha-1 do begin
  wc=where((pradsq ge rlosq(j) and pradsq lt rhisq(j) and $ 
 ph ge chlo(inst,k) and ph le chhi(inst,k)),nwc)
  cts(j,k)=float(nwc)
 endfor
endfor
return
end
