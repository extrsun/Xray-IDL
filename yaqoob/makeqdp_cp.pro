pro makeqdp,slist,blist,bin=bin,ncur=ncur,fname=fname,outname=outname
if n_params(0) eq 0 then begin
 print,' MAKEQDP, slist, blist, bin=bin,fname=fname,outname=outname[rootname]'
 print,' Make a multi-pha range qdp file containing light curves from '
 print,' two photon lists (slist and blist). The idea is that each panel'
 print,' of the plot will consist of a source (slist) and background '
 print,' lightcurve (blist) in a particular PHA range. The PHA ranges '
 print,' are read from a file fname which contains inclusive lower and '
 print,' upper PHA boundaries. Not more than 6 panels recommended per plot'
 print,' Creates output files outname.qdp and outname.pco '
 print,' Simply type <IDL>$qdp outname > to plot '
 retall
end
ibgd=1
;check if there is no background file
if n_elements(blist) eq 0 then ibgd = 0
;print,' ibgd = ',ibgd
if n_elements(fname) eq 0 then begin
 fname=' '
 read,' Enter filename containing PHA boundaries ',fname
endif
if n_elements(outname) eq 0 then begin
 outname=' '
 read,' Enter output QDP filename ',outname
endif
;number of light curves required
if n_elements(ncur) eq 0 then begin
 ncur = 1
 read,' Enter number of light curves required for each photon list ',ncur
endif
if n_elements(bin) eq 0 then begin
 bin=1.
 read,' Enter bin size in seconds ',bin
endif
phlo=intarr(100)
phhi=intarr(100)
ip=0
openr,1,fname
;read the PHA file
while (not(eof(1))) do begin
 readf,1,p1,p2 & phlo(ip) = p1 & phhi(ip) = p2
; print,p1,p2
 ip = ip + 1
endwhile
close,1
print,' PHA boundaries: '
forprint,phlo(0:ncur-1),phhi(0:ncur-1)
;find minimum and maximum photon times
btmin = 1.E35 & btmax = 0.0
stmin = min(slist.time) & stmax = max(slist.time)
if ibgd gt 0 then begin 
btmin = min(blist.time) & btmax = max(blist.time) 
endif
tmin = min([stmin,btmin]) & tmax = max([stmax,btmax])
print,' Min and Max photon times: ',tmin,tmax
ns=(size(slist))(1) & if ibgd gt 0 then nb = (size(blist))(1)
nbins = lonarr(1) & nbins=((tmax-tmin)/bin)+1 & terr = bin/2.
print,' tmin tmax nbins',0.0,tmax-tmin,nbins
tlow = tmin+findgen(nbins)*bin & thigh=tlow+bin & tcen = ((thigh+tlow)/2.)-tmin
openw,2,(outname+'.pco')
openw,3,(outname+'.qdp')
scts=fltarr(ncur,nbins) & sctserr=scts 
if ibgd gt 0 then begin
	bcts=fltarr(ncur,nbins) & bctserr=bcts
endif
cts=fltarr(ncur)
spha=slist.pha & stime=slist.time
for k=0l, ns-1 do begin
	inx = ((stime(k)-tmin)/bin)
	for i=0,ncur-1 do begin
		cts(i) = (spha(k) ge phlo(i))and(spha(k) le phhi(i))
	endfor
	scts(*,inx) = scts(*,inx)+cts
endfor
if ibgd gt 0 then begin
bpha = blist.pha & btime=blist.time
for k=0l, nb-1 do begin
	inx = ((btime(k)-tmin)/bin)
	for i=0,ncur-1 do begin
		cts(i) = (bpha(k) ge phlo(i))and(bpha(k) le phhi(i))
	endfor
	bcts(*,inx) = bcts(*,inx)+cts
endfor
endif
;write QDP header stuff
ymax1=max(scts) & ymax2=0.0
if ibgd gt 0 then ymax2=max(bcts) & ymax=max([ymax1,ymax2])
ymax=1.2*(ymax/bin)
x1=fltarr(ncur) & x2=fltarr(ncur) & y1=x1 & y2=x1
hgt=0.8/ncur
printf,3,'SKIP SINGLE'
printf,3,'PLOT VERTICAL '
PRINTF,3,'READ SERR 1 2 '
PRINTF,3,'@',(outname+'.pco')
nwin=0
for i=0,ncur-1 do begin
	y1(i)=0.1+i*hgt & y2=y1+hgt
	nwin=nwin+1
	PRINTF,2,'WINDOW ',nwin
	PRINTF,2,'LOC ',0.1,y1(i),0.9,y2(i)
	PRINTF,2,'VIEW 0 0 0 0 '
	PRINTF,2,'R Y -0.1 ',YMAX
	if i eq 0 then PRINTF,2,'LAB X Time - ',tmin,' (secs) '
	if i eq 0 then PRINTF,2,'LA F '
	if i gt 0 then PRINTF,2,'LA NX OFF ',nwin
	PRINTF,2,'LAB Y cts/s '
	printf,2,'MARKER SIZE 0.5 ON ',NWIN
	PRINTF,2,'MARKER 17 ON ',NWIN
endfor
if ibgd gt 0 then begin
  for i=0,ncur-1 do begin
	nwin=nwin+1
	PRINTF,2,'WINDOW ',nwin
	PRINTF,2,'LOC ',0.1,y1(i),0.9,y2(i)
	PRINTF,2,'VIEW 0 0 0 0 '
	PRINTF,2,'R Y -0.1 ',YMAX
	PRINTF,2,'LA NX OFF ',nwin
  endfor
endif
PRINTF,2,'LA NX ON 1'
PRINTF,2,'WINDOW ',(1+ibgd)*NCUR
PRINTF,2,'LA T START TIME AND BINSIZE = ',tmin,bin,' (secs)'
for k=0,ncur-1 do begin
for j=0l,nbins-1 do begin
	if scts(k,j) gt 0 then sctserr(k,j)=sqrt(scts(k,j))/bin
	scts(k,j) = scts(k,j)/bin
	if scts(k,j) le 0.0 then scts(k,j) = -10.0
	printf,3,tcen(j),terr,scts(k,j),sctserr(k,j)
endfor
	printf,3,'no   no   no   no'
if ibgd gt 0 then begin
for j=0l,nbins-1 do begin
	if bcts(k,j) gt 0 then bctserr(k,j)=sqrt(bcts(k,j))/bin
	bcts(k,j) = bcts(k,j)/bin
	if bcts(k,j) le 0.0 then bcts(k,j) = -10.0
	printf,3,tcen(j),terr,bcts(k,j),bctserr(k,j)
endfor
	printf,3,'no   no   no   no'
endif
endfor
;for i=0,ncur-1 do begin
;  tslist = slist(where((slist.pha ge phlo(i)) and (slist.pha le phhi(i))))
;  for j=0,nbins-1 do begin
;	tm=tslist.time
;	y = total((tm ge tlow(j)) and (tm lt thigh(j)))
;	if y gt 0 then yerr = sqrt(y)/bin
;	if y le 0 then yerr = 0.0
;	& y=y/bin
;	printf,2,tcen(j),terr,y,yerr
;	print,tcen(j),terr,y,yerr
;endfor
;  printf,2,'no	  no   no   no '
;  if ibgd gt 0 then begin
; tblist = blist(where((slist.pha ge phlo(i)) and (slist.pha le phhi(i))))
;	for j=0,nbins-1 do begin
;	tm=tblist.time
;	y = total((tm ge tlow(j)) and (tm lt thigh(j)))
;	if y gt 0 then yerr = sqrt(y)/bin
;	if y le 0 then yerr = 0.0
;	y = y/bin
;	printf,2,tcen(j),terr,y,yerr
;	print,tcen(j),terr,y,yerr	
;	endfor
;	printf,2,'no   no   no   no '
; endif
;endfor
close,2
close,3
return
end
