pro sispi,plist,sistype,dname=dname,fname=fname,nom_gain=nom_gain,h=h,$
	verbose=verbose

	if n_params(0) eq 0 then begin
	  print,'sispi,plist,sistype,dname=dname,fname=fname'
;	  print,'Converts pha in inlist to pi, stored as pha in outlist'
	  print,'Fills pi column in plist'
	  print,'dname//fname specifies conversion caldb fits file'
	  print,'default = /ftools/DEC/release/refdata/sisph2pi.fits'
	  print,'non_gain = nominal gain, default = 3.65 used by pi rmfs'
	  return
	endif
	
	if not keyword_set(dname) then dname='/home/astd1/yaqoob/siscal/'
	if not keyword_set(fname) then fname='sisph2pi.fits'
	if not keyword_set(nom_gain) then nom_gain=3.65
	v = keyword_set(verbose)

	tab = readfits(dname+fname,hdr,ext=1,/silent)
; figure out which set of coefficients to use
	time = tbget(hdr,tab,'time')
	nel_t = n_elements(time)
	if v then print,'Time values in ',fname,': ',strn(time)
	mm = dblarr(2)
	if keyword_set(h) then begin
	  mm(0) = sxpar(h,'tstart')
	  mm(1) = sxpar(h,'tstop')
	endif else mm = minmax(plist.time)
	midt = (mm(0)+mm(1))*0.5
	if v then print,mm,midt
	w0 = max(where(mm(0) ge time))
	w1 = max(where(mm(1) ge time))
	if w0 ne w1 then begin
	  print,'Events in plist span more than one coef. time range'
	  stop
	endif
	c = dblarr(6,4)	  ; coef,ccd
	sis = 's'+strn(sistype)
; if not at endpoints, interpolate coefs.
	if w1 lt nel_t-1 and w1 gt 0 then begin
	  delta_t = time(w1+1)-time(w1)
	  frac1 = (time(w1+1)-midt)/delta_t
	  frac2 = (midt-time(w1))/delta_t
	  for i=0,3 do begin
	    fld = sis+'c'+strn(i)
	    c0 = tbget(hdr,tab,fld)
	    if v then print,fld
	    c(*,i) = c0(*,w1)*frac1+c0(*,w1+1)*frac2
	  endfor 
	endif else for i=0,3 do begin
	  fld = sis+'c'+strn(i)
	  c0 = tbget(hdr,tab,fld)
	  if v then print,fld
	  c(*,i) = c0(*,w1)
	endfor
	if v then for i=0,3 do begin
; look at change in gain across chip
	  print,'chip '+strn(i),c(0,i)+200.0*c(1,i)+c(2,i),$
		c(3,i)+200.0*c(4,i)+c(5,i),c(0,i)+200.0*c(1,i)+422.0*c(2,i),$
		c(3,i)+200.0*c(4,i)+422.0*c(5,i)
	endfor
	if v then print,"$(6(f10.7,2x))",c
;	seed = 23019l
;	seed = sxpar(hdr,'ran_seed')
	seed = long(systime(1))
	if v then print,'Seed = ',seed
	for i=0,n_elements(plist)-1 do begin
;	  pha =  double(plist(i).pha) + 0.5
	  pha =  double(plist(i).pha) + randomu(seed,1)-0.5
; go to 4096 channels
	  pha = pha + max([pha-1024,0])+max([pha-1536,0])*2
	  ccd = plist(i).ccd
	  x = plist(i).x
	  y = plist(i).y
	  pi = fix((pha*(c(0,ccd)+c(1,ccd)*x+c(2,ccd)*y)+c(3,ccd)+c(4,ccd)*x+$
		c(5,ccd)*y)/nom_gain)
; go back to 2048
	  plist(i).pi = pi(0) - max([pi(0)-1023,0])/2 - max([pi(0)-2046,0])/4 
	endfor
	return
	end







