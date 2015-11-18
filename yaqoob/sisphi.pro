pro sisphi,plist,sistype,dname=dname,fname=fname,fchp=fchp,nom_gain=nom_gain,$
	verbose=verbose
; ** Authors A. Ptak and T. Yaqoob
; ** Last modified 18 Oct 1995
	if n_params(0) eq 0 then begin
	  print,'sisphi,plist,sistype,dname=dname,fname=fname,fchp=fchp,nom_gain=nom_gain,verbose=verbose'
	  print,'Converts pha in SIS plist to pi'
	  print,'Fills pi column in plist'
	  print,'dname//fname specifies conversion caldb fits file'
;	  print,'default = /ftools/DEC/release/refdata/sisph2pi.fits'
	  print,'default = /home/astd1/yaqoob/siscal/sisph2pi.fits'
	  print,'non_gain = nominal gain, default = 3.65 used by pi rmfs'
	  print,'FCHP=0 means the coords plist.x y are truly RAW'
	  print,'FCHP=1 means  plist.x y are mock-up 4 chip coords'
	  return
	endif
	if not keyword_set(dname) then dname='/home/astd1/yaqoob/siscal/'
	if not keyword_set(fname) then fname='sisph2pi.fits'
	if not keyword_set(nom_gain) then nom_gain=3.65
        print,'Using NOM_GAIN = ',nom_gain
	v = keyword_set(verbose)
	np=(size(plist))(1)
	ptim=plist.time +0.d0
	c = dblarr(np,6,4)
	tab = readfits(dname+fname,hdr,ext=1,/silent)
; figure out which set of coefficients to use
	time = tbget(hdr,tab,'time')+0.d0
	nel_t = n_elements(time)
	if v then print,'Time values in ',fname,': ',strn(time)
;step through each time interval in the sis pi file and produce
;interpolated coefficients
 	for j=0l,nel_t-3l do begin
	  t1=time(j) & t2=time(j+1)
	  delt=t2-t1
	  wt=where((ptim ge t1 and ptim le t2),nwt)
	  if nwt gt 0 then begin
;each chip
	    for i=0,3 do begin
		fld = 's'+strn(sistype)+'c'+strn(i)
		ctmp=tbget(hdr,tab,fld)+0.d0
;each coefficient
		for k=0,5 do begin
		 y1=ctmp(k,j) & y2=ctmp(k,j+1)
	 	c(wt,k,i)=y1 + (ptim(wt)-t1)*(y2-y1)/delt
		endfor
	    endfor
	   endif
	endfor 
;now do the last time interval assuming we only believe the penultimate
;time in the sis pi file. Thus we extrapolate using either the same
;slope is in the previous interval or we make up a slope.
	wt=where((ptim ge time(nel_t-2)),nwt)
	if nwt gt 0 then begin
	  for i=0,3 do begin
	   	fld = 's'+strn(sistype)+'c'+strn(i)
		 ctmp=tbget(hdr,tab,fld)+0.d0
		for k=0,5 do begin
;put your own slope here
 slope=(ctmp(k,nel_t-1)-ctmp(k,nel_t-2))/(time(nel_t-1)-time(nel_t-2))
		 y2=ctmp(k,nel_t-1)  
		c(wt,k,i)=y2 + slope*(ptim(wt)-time(nel_t-1))
		endfor
	  endfor
	endif
; if not at endpoints, interpolate coefs.
;	if v then for i=0,3 do begin
; look at change in gain across chip
;	  print,'chip '+strn(i),c(0,i)+200.0*c(1,i)+c(2,i),$
;		c(3,i)+200.0*c(4,i)+c(5,i),c(0,i)+200.0*c(1,i)+422.0*c(2,i),$
;		c(3,i)+200.0*c(4,i)+422.0*c(5,i)
;	endfor
;	if v then print,"$(6(f10.7,2x))",c
;	seed = 23019l
;	seed = sxpar(hdr,'ran_seed')
	seed = long(systime(1))
	if v then print,'Seed = ',seed
	xt=plist.x
	yt=plist.y
;convert from mock-up 4-chip raw coords to true raw coords 
	  if fchp gt 0 then begin
		 fchp2raw,xt,yt,x,y,sistype,plist.ccd
	  endif else begin
		x=xt & y=yt
	  endelse	
	 print,'MIMAX x ' ,minmax(x) 
	 print,'MIMAX y ' ,minmax(y) 
	pha=plist.pha+randomu(seed,np)
	pi=pha*0.0
	wge=where((pha gt 2047),nwge)
 	if nwge gt 0 then pha(wge)=lonarr(nwge)+2047
	wp1=where((pha le 1023),nwp1) 
	wp2=where((pha gt 1023 and pha le 1535),nwp2) 
	wp3=where((pha gt 1535 and pha le 2047),nwp3) 
;convert to 4096 chan spectrum
	if nwp2 gt 0 then begin
	  pha(wp2)=pha(wp2)+pha(wp2)-1023$
	-(fix(randomu(seed)+0.5))
	endif
	if nwp3 gt 0 then begin
	 pha(wp3)=pha(wp3)+pha(wp3)-1023$
	 +2*(pha(wp3)-1535)-fix(4.*randomu(seed,nwp3))
	endif
	for ic=0,3 do begin
	 wc=where((plist.ccd eq ic),nwc)
	 if nwc gt 0 then begin
	  c0=c(wc,0,ic) & c1=c(wc,1,ic) & c2=c(wc,2,ic) 
	  c3=c(wc,3,ic) & c4=c(wc,4,ic) & c5=c(wc,5,ic)
;	print,'coefficients c0-c2 are for chip ',ic
;	print,minmax(c0)
;	print,minmax(c1)
; 	print,minmax(c2)
;	print,'X and Y range is'
;	print,minmax(x(wc))
; 	print,minmax(y(wc))
;	print,'Mult factor is '
;	print,minmax(c0+c1*x(wc)+c2*y(wc))
pi(wc)=fix((pha(wc)*(c0+c1*x(wc)+c2*y(wc))+c3+$
 c4*x(wc)+c5*y(wc))/nom_gain)
	 endif
	endfor
;bin back up to 2048 channels
 pi=pi*(pi le 1024)+(512+pi/2)*((pi gt 1024) and (pi le 2048))$
 +(1024+pi/4)*((pi gt 2048)and(pi le 4096))
	plist.pi=pi
	return
	end
