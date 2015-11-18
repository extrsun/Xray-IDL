pro makeqdp,bscale,slist,stint,blist,btintin,bin=bin,ncur=ncur,fname=fname,$
outname=outname,reftime=reftime,xb=xb,exp=exp
;Author T. Yaqoob and A. Ptak - March 1993 ->**
if n_params(0) eq 0 then begin
 print,' MAKEQDP,bscale,slist,stint,blist,btint,bin=bin,ncur=ncur,fname=fname,outname=outname[rootname],reftime=reftime,xb=xb,exp=exp'
 print,' Make a multi-pha range qdp file containing light curves from '
 print,' two photon lists (slist and blist). The idea is that each panel'
 print,' of the plot will consist of a source (slist) and background '
 print,' lightcurve (blist) in a particular PHA range. The PHA ranges '
 print,' are read from a file fname which contains inclusive lower and '
 print,' upper PHA boundaries. Not more than 6 panels recommended per plot'
 print,' BSCALE is the background area scaling factor '
 print,' Creates output files outname.qdp and outname.pco '
 print,' Simply type <IDL>$qdp outname > to plot '
 print,' NOTE: stint(*,2) and btint(*,2) are arrays of Good Time Intervals for '
 print,' slist and blist respectively '
 print,'REFTIME : reference (ASCA) time time for light curve start ' 
 print,'XB == PHA/PI band to plot as X-axis in hardness ratio plots '
 print,'exp = exposure of bins in final plot, i.e., keep onlys bins with'
 print,' time > exp*bin'
 print,' '
 print,'Your output from MAKEQDP will consist of up to 5 QDP files'
 print,'Tou specify the ROOTNAME and the files will be '
 print,'- ROOTNAME.QDP - cts/s vs. time with SRC+BGD and BGD in each panel'
 print,'- ROOTNAME_HRD.QDP - bgd subtracted cts/s vs bgd subtracted cts/s '
 print,'		   - (you choose which band is the x-axis with XB'
 print,'- ROOTNAME_RAT.QDP - bgd subtracted cts/s ratios in all bands rel. '
 print,'		   - to cts/s in band 1 versus cts/s in band 1'
 print,'- ROOTNAME_RTM.QDP - The above hardness ratios versus time '
 print,'- ROOTNAME_BS.QDP  - Background subtracted lightcurves vs. time '
 retall
end
if bscale le 0.0 then bscale=1.0
if n_elements(exp) eq 0 then exp = 0.0
ibgd=1
;check if there is no background file
if n_elements(blist) eq 0 then ibgd = 0
;print,' ibgd = ',ibgd
if n_elements(fname) eq 0 then begin
 fname=' '
 read,' Enter filename containing PHA/PI boundaries for slist',fname
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
read,' Enter 0 if slist channel bounds are PHA & 1 if PI ',iphs
phlo=intarr(100) & phlob=intarr(100)
phhi=intarr(100) & phhib=intarr(100)
labl = strarr(100)
la = ''
ip=0
openr,1,fname
;read the PHA file
while not (eof(1)) do begin
 readf,1,p1,p2,la 
 phlo(ip) = p1 & phhi(ip) = p2 & phlob(ip)=p1 & phhib(ip)=p2
 labl(ip) = strtrim(la,2)
; print,p1,p2
 ip = ip + 1
endwhile
close,1
print,' PHA boundaries: '
for iz=0,ncur-1 do print,phlo(iz),phhi(iz)
if n_elements(xb) eq 0 then begin
 read,'Enter the number of energy band to use as X-axis (XB)',xb
 xb=xb-1
endif
;find minimum and maximum photon times
btmin = 1.E35 & btmax = 0.0
stmin = min(slist.time) & stmax = max(slist.time)
if ibgd gt 0 then begin 
fbname=' '
read,' Enter file name containing PHA/PI bounds for blist[or same] ',fbname
if fbname ne 'same' then begin
ip=0
openr,1,fbname
;read the PHA file
while not (eof(1)) do begin
 readf,1,p1,p2 
 phlob(ip)=p1 & phhib(ip)=p2
; print,p1,p2
 ip = ip + 1
endwhile
close,1
print,' PHA boundaries: '
for iz=0,ncur-1 do print,phlob(iz),phhib(iz)
endif
read,' Enter 0 if blist channel bounds are PHA & 1 if PI ',iphb
print,' Relative offset between blist and slist: '
print,' Example: if blist is ROSAT data and slist ASCA data offset is 81572062.0 s'
print,'To map ASCA time onto  XTE, offset= -31536063.562'
read,' Enter relative offset (secs) ',boffset
btmin = min(blist.time-boffset) & btmax = max(blist.time-boffset) 
endif
tmin = min([stmin,btmin]) & tmax = max([stmax,btmax])
print,' Min and Max photon times: ',tmin,tmax
if n_elements(reftime) eq 0 then reftime=tmin
 print,' Using reference time ',reftime
ns=(size(slist))(1) & if ibgd gt 0 then nb = (size(blist))(1)
nbins = lonarr(1) & nbins=fix((tmax-tmin)/bin)+1 & terr = bin/2.
print,' tmin tmax nbins',0.0,tmax-tmin,nbins
tlow = tmin+findgen(nbins)*bin & thigh=tlow+bin 
tcen = ((thigh+tlow)/2.)-reftime
thalf=fltarr(nbins)
openw,2,(outname+'.pco')
openw,3,(outname+'.qdp')
openw,4,(outname+'_hrd.qdp')
openw,5,(outname+'_rat.qdp')
openw,6,(outname+'_rtm.qdp')
scts=fltarr(ncur,nbins) & sctserr=scts & rat=scts & raterr=scts 
sbcts=scts & sbctserr=scts
if ibgd gt 0 then begin
 bcts=fltarr(ncur,nbins) & bctserr=bcts
 subcts=bcts & subctserr=bcts & mbgd=fltarr(ncur) & mbgderr=mbgd
endif
cts=fltarr(ncur)
if iphs eq 0 then spha=slist.pha else spha = slist.pi 
stime=slist.time
for k=0l, ns-1 do begin
	inx = ((stime(k)-tmin)/bin)
	for i=0,ncur-1 do begin
		cts(i) = (spha(k) ge phlo(i))and(spha(k) le phhi(i))
	endfor
	scts(*,inx) = scts(*,inx)+cts
endfor
if ibgd gt 0 then begin
if iphb eq 0 then bpha = blist.pha else bpha=blist.pi
btime=blist.time-boffset & btint=btintin-boffset
for k=0l, nb-1 do begin
	inx = ((btime(k)-tmin)/bin)
	for i=0,ncur-1 do begin
		cts(i) = (bpha(k) ge phlob(i))and(bpha(k) le phhib(i))
	endfor
	bcts(*,inx) = bcts(*,inx)+cts
endfor
endif
;write QDP header stuff
ymax1=max(scts) & ymax2=0.0
if ibgd gt 0 then ymax2=max(bcts/bscale) & ymax=max([ymax1,ymax2])
ymax=1.2*((ymax+sqrt(ymax))/bin)
x1=fltarr(ncur) & x2=fltarr(ncur) & y1=x1 & y2=x1
hgt=0.8/ncur
printf,3,'!SOURCE+BGD AND BGD LIGHTCURVES FROM MAKEQDP '
PRINTF,3,'!MAKEQDP written by T. Yaqoob & A. Ptak 1993-1995'
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
	PRINTF,2,'R Y -0.01 ',YMAX
	if i eq 0 then PRINTF,2,'LAB X Time - ',reftime,' (secs) '
	if i eq 0 then PRINTF,2,'LA F '
	if i gt 0 then PRINTF,2,'LA NX OFF ',nwin
	PRINTF,2,'LAB Y cts/s '
	PRINTF,2,'LAB OY '+labl(i)
	printf,2,'MARKER SIZE 0.5 ON ',NWIN
	PRINTF,2,'MARKER 17 ON ',NWIN
if ibgd gt 0 then begin
	nwin=nwin+1
	PRINTF,2,'WINDOW ',nwin
	PRINTF,2,'LOC ',0.1,y1(i),0.9,y2(i)
	PRINTF,2,'VIEW 0 0 0 0 '
	PRINTF,2,'R Y -0.01 ',YMAX
	PRINTF,2,'LA NX OFF ',nwin
endif
endfor
PRINTF,2,'LA NX ON 1'
PRINTF,2,'WINDOW ',(1+ibgd)*NCUR
PRINTF,2,'LA T START TIME AND BINSIZE = ',reftime,bin,' (secs)'
tint_1=dblarr(1,2)
for k=0,ncur-1 do begin
print,' Doing panel ',k+1
for j=0l,nbins-1 do begin
		terr=0.0d0 & texp = 0.0d0
;compute proper time bin width
	if scts(k,j) gt 0 then begin
;compute effective exposure for this bin
	  tint_1(0,0)=tlow(j) & tint_1(0,1)=thigh(j)
	  texp=0.0d0 & combtint,tint_1,stint,tout & ntout=(size(tout))(1)
	  if ntout gt 0 then begin
	   for ii=0l,ntout-1 do texp=texp+(tout(ii,1)-tout(ii,0))
	   sctserr(k,j)=sqrt(scts(k,j))/texp
	   scts(k,j) = scts(k,j)/texp
	   terr=texp/2. & thalf(j)=terr 
	  endif
	 endif
	if scts(k,j) le 0.0 or texp lt exp*bin then begin
	 scts(k,j) = -10.0 & terr=bin/2. & thalf(j)=terr
	 sctserr(k,j) = 0.0
	endif
	if scts(k,j) gt 0.0 then printf,3,tcen(j),terr,scts(k,j),sctserr(k,j)
endfor
	printf,3,'no   no   no   no'
if ibgd gt 0 then begin
for j=0l,nbins-1 do begin
	terr=0.0d0 
;compute proper time bin width
	if bcts(k,j) gt 0 then begin
;compute effective exposure for this bin
	  tint_1(0,0)=tlow(j) & tint_1(0,1)=thigh(j)
	  texp=0.0d0 & combtint,tint_1,btint,tout & ntout=(size(tout))(1)
	  if ntout gt 0 then begin
	   for ii=0l,ntout-1 do texp=texp+(tout(ii,1)-tout(ii,0))
	   bctserr(k,j)=sqrt(bcts(k,j)/bscale)/texp
	   bcts(k,j) = bcts(k,j)/texp/bscale
	   terr=texp/2.  
	  endif
	 endif
	if bcts(k,j) le 0.0 or texp lt exp*bin then begin
	 bcts(k,j) = -10.0 & terr=bin/2.
	 bctserr(k,j) = 0.0
	endif
	printf,3,tcen(j),terr,bcts(k,j),bctserr(k,j)
endfor
	printf,3,'no   no   no   no'
endif
endfor
if ibgd gt 0 and ncur gt 1 then begin
	PRINTF,4,'!CTS/S IN VARIOUS ENERGY BANDS PLOTTED AGAINST CTS/S '
 	PRINTF,4,'!IN THE REMAINING ENERGY BAND. THE COUNT-RATE ARE '
	PRINTF,4,'!CORRECTED BY SUBTRACTING THE MEAN BACKGROUND LEVEL '
        PRINTF,4,'!THIS IS AN OUTPUT FILE FROM MAKEQDP '
 	PRINTF,4,'!MAKEQDP written by T. Yaqoob & A. Ptak 1993-1995'
	printf,4,'SKIP SINGLE'
	printf,4,'READ SERR 1 2'
	printf,4,'PLOT VERTICAL '
	delwn=0.8/float(ncur-1)
	for kk=0,ncur-2 do begin
	 printf,4,'WIN ',kk+1
	 lowc = 0.9-float(kk+1)*delwn
	 highc = 0.9-float(kk)*delwn
	 printf,4,'LOC 0.3 ',lowc,0.7,highc
	 printf,4,'VIEW 0 0 0 0'
	 printf,4,'R 0.0 1.0 0.0 1.0'
	 printf,4,'LA Y band ',ncur-kk-1
	 if (kk+1) eq (ncur-1) then printf,4,'LA X X-Y keV cts/s'
        endfor
	for kk = 0,ncur-2 do begin
	 PRINTF,4,'MARKER 17 ON ',kk+1
	endfor
	PRINTF,5,'!HARDNESS RATIOS RELATIVE TO ENERGY BAND 1 VESRUS CTS/S '
 	PRINTF,5,'!IN ENERGY BAND 1. THE RATIOS ARE COMPUTED BY FIRST '
	PRINTF,5,'!SUBTRACTING THE MEAN BACKGROUND LEVEL FROM EACH BAND '
        PRINTF,5,'!THIS IS AN OUTPUT FILE FROM MAKEQDP '
 	PRINTF,5,'!MAKEQDP written by T. Yaqoob & A. Ptak 1993-1995'
	printf,5,'SKIP SINGLE'
	printf,5,'PLOT VERTICAL'
	for kk=0,ncur-2 do begin
	 printf,5,'WIN ',kk+1
                     lowc = 0.9-float(kk+1)*delwn
                     highc = 0.9-float(kk)*delwn
                     printf,5,'LOC 0.3 ',lowc,0.7,highc
                     printf,5,'VIEW 0 0 0 0'
	 printf,5,'R 0.0 1.0 0.0 1.0'
	 printf,5,'LA OY ratio ',ncur-kk-1
	 printf,5,'LA Y (',labl(kk+1),')/(',labl(0),')'
;	 if (kk+1) eq ncur then printf,5,'LA X X-Y keV cts/s'
	 printf,5,'LA X ',labl(xb),' cts/s'
	endfor
	printf,5,'READ SERR 1 2'
	for kk = 0,ncur-2 do begin
	 PRINTF,5,'MARKER 17 ON ',kk+1
	endfor
	PRINTF,6,'!HARDNESS RATIOS RELATIVE TO ENERGY BAND 1 CTS/S VESRUS TIME '
 	PRINTF,6,'!THE RATIOS ARE COMPUTED BY FIRST '
	PRINTF,6,'!SUBTRACTING THE MEAN BACKGROUND LEVEL FROM EACH BAND '
        PRINTF,6,'!THIS IS AN OUTPUT FILE FROM MAKEQDP '
 	PRINTF,6,'!MAKEQDP written by T. Yaqoob & A. Ptak 1993-1995'
	printf,6,'SKIP SINGLE'
	printf,6,'PLOT VERTICAL'
	printf,6,'READ SERR 1 2'
	for kk=0,ncur-2 do begin
	 printf,6,'WIN ',kk+1
		lowc = 0.9-float(kk+1)*delwn
		highc = 0.9-float(kk)*delwn
		printf,6,'LOC 0.3 ',lowc,0.7,highc
		printf,6,'VIEW 0 0 0 0'
		printf,6,'LA X Time - ',reftime,' (secs)'
		printf,6,'LA OY ratio ',ncur-kk-1
	 	printf,6,'LA Y (',labl(kk+1),')/(',labl(0),')'
		PRINTF,6,'MARKER 17 ON ',kk+1
	endfor
;weight mean background for 1st light curve
       wgtmean,bcts(0:0,0:nbins-1),bctserr(0:0,0:nbins-1),bmean,bsigma
	mbgd(0)=bmean & mbgderr(0)=bsigma
	print,'Mean background for 	1',mbgd(0),'+/- ',mbgderr(0)
	subcts(0:0,0:nbins-1)=scts(0:0,0:nbins-1)-mbgd(0)
	subctserr(0:0,0:nbins-1)=sqrt(sctserr(0:0,0:nbins-1)*sctserr(0:0,0:nbins-1)$
	+mbgderr(0)*mbgderr(0))
	for k=1,ncur-1 do begin
	  printf,4,'! curve ',k,' (scts-bgd )(0) (scts-bgd err)(0)  scts  sctserr '
;compute background subracted counts
	  wgtmean,bcts(k:k,0:nbins-1),bctserr(k:k,0:nbins-1),bmean,bsigma
	  mbgd(k)=bmean & mbgderr(k)=bsigma
	  print,'Mean background for ',k+1,mbgd(k),'+/- ',mbgderr(k)
	  subcts(k:k,0:nbins-1)=scts(k:k,0:nbins-1)-mbgd(k)
	  subctserr(k:k,0:nbins-1)=sqrt(sctserr(k:k,0:nbins-1)*sctserr(k:k,0:nbins-1)$
	 +mbgderr(k)*mbgderr(k))
;	  for j=0l,nbins-1 do begin
;	   printf,4,subcts(0,j),subctserr(0,j),subcts(k,j),subctserr(k,j)
;	  endfor
	  printf,5,'! ratio of subcts(k)/subcts(0) for curve ',k+1
	  for j=0l,nbins-1 do begin
	   if subcts(k,j) gt 0.0 and subcts(0,j) gt 0.0 then begin
	     rat(k,j) = subcts(k,j)/subcts(0,j)
	     raterr(k,j) = rat(k,j)*sqrt((subctserr(k,j)/subcts(k,j))^2.+$
(subctserr(0,j)/subcts(0,j))^2.)
;	     printf,5,subcts(0,j),subctserr(0,j),rat(k,j),raterr(k,j)
;            printf,6,tcen(j),thalf(j),rat(k,j),raterr(k,j)
	   endif
	  endfor
	endfor
	for k=0,ncur-1 do begin
         if (k ne xb) then begin
 	  for j=0l,nbins-1 do begin
            printf,4,subcts(xb,j),subctserr(xb,j),subcts(k,j),subctserr(k,j)
	  endfor
	    printf,4,'no   no     no      no'
	 endif
	 if k gt 0 then begin
 	  for j=0l,nbins-1 do begin
	    if rat(k,j) gt 0.0 then begin
	     printf,5,subcts(xb,j),subctserr(xb,j),rat(k,j),raterr(k,j)
	     printf,6,tcen(j),thalf(j),rat(k,j),raterr(k,j)
	    endif
	  endfor
	    printf,5,'no   no     no      no'
	    printf,6,'no   no     no      no'
	 endif
	endfor
endif
if ibgd le 0 and ncur gt 1 then begin
	PRINTF,4,'!CTS/S IN NCUR-1 ENERGY BANDS VERSUS CTS/S IN REMAINING BAND '
 	PRINTF,4,'!THE COUNT-RATES ARE CORRECTED BY  '
	PRINTF,4,'!SUBTRACTING THE MEAN BACKGROUND LEVEL FROM EACH BAND '
        PRINTF,4,'!THIS IS AN OUTPUT FILE FROM MAKEQDP '
 	PRINTF,4,'!MAKEQDP written by T. Yaqoob & A. Ptak 1993-1995'
	printf,4,'SKIP SINGLE'
	printf,4,'PLOT VERTICAL'
 	printf,4,'READ SERR 1 2'
	delwn=0.8/float(ncur-1)
	for kk=0,ncur-2 do begin
	 printf,4,'WIN ',kk+1
	 lowc = 0.9-float(kk+1)*delwn
	 highc = 0.9-float(kk)*delwn
	 printf,4,'LOC 0.3 ',lowc,0.7,highc
	 printf,4,'VIEW 0 0 0 0'
	 printf,4,'R 0.0 1.0 0.0 1.0'
	 printf,4,'LA Y band ',ncur-kk-1
	 if (kk+1) eq ncur then printf,4,'LA X X-Y keV cts/s'
                  endfor
	PRINTF,5,'!HARDNESS RATIOS RELATIVE TO ENERGY BAND 1 VESRUS CTS/S '
 	PRINTF,5,'!IN ENERGY BAND 1. THE RATIOS ARE COMPUTED BY FIRST '
	PRINTF,5,'!SUBTRACTING THE MEAN BACKGROUND LEVEL FROM EACH BAND '
        PRINTF,5,'!THIS IS AN OUTPUT FILE FROM MAKEQDP '
 	PRINTF,5,'!MAKEQDP written by T. Yaqoob & A. Ptak 1993-1995'
	printf,5,'SKIP SINGLE'
	printf,5,'READ SERR 1 2'
	printf,5,'PLOT VERTICAL'
	printf,6,'READ SERR 1 2'
 	printf,6,'SKIP SINGLE'
 	printf,6,'PLOT VERT'
	for kk=0,ncur-2 do begin
	 printf,5,'WIN ',kk+1
                     lowc = 0.9-float(kk+1)*delwn
                     highc = 0.9-float(kk)*delwn
                     printf,5,'LOC 0.3 ',lowc,0.7,highc
                     printf,5,'VIEW 0 0 0 0'
	 printf,5,'R 0.0 1.0 0.0 1.0'
	 printf,5,'LA Y ratio ',ncur-kk-1
	 if (kk+1) eq ncur then printf,4,'LA X X-Y keV cts/s'
	endfor
	for kk = 0,ncur-2 do begin
	 PRINTF,5,'MARKER 17 ON ',kk+1
	endfor
	for kk=0,ncur-2 do begin
		PRINTF,4,'MARKER 17 ON ',KK+1
	endfor
	for k=1,ncur-1 do begin
	  printf,4,'! curve ',k+1,' scts  sctserr  vs curve 1 scts and sctserr '
;	  for j=0l,nbins-1 do begin
;		printf,4,scts(0,j),sctserr(0,j),scts(k,j),sctserr(k,j)
;	  endfor
	  printf,5,'! ratio of scts/scts(1) for curve ',k+1
	  for j=0l,nbins-1 do begin
	   if scts(k,j) gt 0.0 and scts(k-1,j) gt 0.0 then begin
		rat(k,j) = scts(k,j)/scts(0,j)
		raterr(k,j) = rat(k,j)*sqrt((sctserr(k,j)/scts(k,j))^2.+$
(sctserr(0,j)/scts(0,j))^2.)
;		printf,5,scts(0,j),sctserr(0,j),rat,raterr
	   endif
	  endfor
	endfor
	for k=0,ncur-1 do begin
         if k ne xb then begin
	  for j=0l,nbins-1 do begin
	   printf,4,scts(xb,j),sctserr(xb,j),scts(k,j),sctserr(k,j)
	  endfor
	  printf,4,'no 	 no 	no 	no'
         endif
	 if k gt 0 then begin
	  for j=0l,nbins-1 do begin
	  if rat(k,j) gt 0.0 then begin
	   printf,5,scts(xb,j),sctserr(xb,j),rat(k,j),raterr(k,j)
	   printf,6,tcen(j),thalf(j),rat(k,j),raterr(k,j)
	  endif
	  endfor
	  printf,5,'no 	 no 	no 	no'
	  printf,6,'no 	 no 	no 	no'
	 endif
	endfor
endif
close,2
close,3
close,4
close,5
close,6
if ibgd gt 0 then begin
openw,7,(outname+'_bs.qdp')
PRINTF,7,'!BGD-SUBTRACTED LIGHTCURVE FROM MAKEQDP '
PRINTF,7,'!THE BACKGROUND IN CORRESPONDING GTIs IS SUBTRACTED DIRECTLY'
PRINTF,7,'!MAKEQDP written by T. Yaqoob & A. Ptak 1993-1995 '
PRINTF,7,'SKIP SINGLE'
PRINTF,7,'PLOT VERTICAL '
PRINTF,7,'READ SERR 1 2'
nwin=0
for i=0,ncur-1 do begin
	y1(i)=0.1+i*hgt & y2=y1+hgt
	nwin=nwin+1
	PRINTF,7,'WINDOW ',nwin
	PRINTF,7,'LOC ',0.1,y1(i),0.9,y2(i)
	PRINTF,7,'VIEW 0 0 0 0 '
	PRINTF,7,'R Y -0.01 ',YMAX
	if i eq 0 then PRINTF,7,'LAB X Time - ',reftime,' (secs) '
	if i eq 0 then PRINTF,7,'LA F '
	if i gt 0 then PRINTF,7,'LA NX OFF ',nwin
	PRINTF,7,'LAB Y cts/s '
	PRINTF,7,'LAB OY '+labl(i)
	printf,7,'MARKER SIZE 0.5 ON ',NWIN
	PRINTF,7,'MARKER 17 ON ',NWIN
endfor
PRINTF,7,'LA NX ON 1'
PRINTF,7,'WINDOW ',NCUR
PRINTF,7,'LA T START TIME AND BINSIZE = ',reftime,bin,' (secs)'
PRINTF,7,'LA OT BACKGROUND-SUBTRACTED LIGHT CURVE '
 for k=0,ncur-1 do begin
  for j=0l,nbins-1 do begin
   if scts(k,j) gt 0.0 and bcts(k,j) ge 0.0 then begin
    sbcts(k,j) = scts(k,j)-bcts(k,j)
    sbctserr(k,j) = sctserr(k,j)*sctserr(k,j)+bctserr(k,j)*bctserr(k,j)
    if sbctserr(k,j) gt 0.0 then sbctserr(k,j)=sqrt(sbctserr(k,j))
    if sbcts(k,j) gt 0.0 then printf,7,tcen(j),thalf(j),sbcts(k,j),sbctserr(k,j)
   endif
  endfor
  printf,7,'NO NO NO NO'
endfor
close,7
endif
return
end
