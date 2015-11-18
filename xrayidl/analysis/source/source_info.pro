pro source_info,souno,ra,dec,ston,cntr,dif,dis,xp,yp $
,soufile=soufile,ns=n_source,slow=slow,flow=flow,text=text $
,deg=deg,xshift=xshift,yshift=yshift,ashift=ashift,crval=crval,deci=deci,perr=perr
;+
; filter and read source parameters from source lists created by Daniel's 
; source detection programs
;
;*Inputs:
; soufile - source list file name
; slow - lower S/N threshold for the source filtering
; flow - lower flux threshold
; xshift, yshift, ashift - X, Y, and angular shifts of source positions
; 	before the output
; crval - 2-element vector containing the RA and Dec of the rotation center
; deci - if set, ra and dec will directly come from decimal values enclosed
;		within & and & in the input source file (which could be
;		more accurate). 
;
;*Outputs:
; souno - source number (first column)
; ra, dec - source RA and Dec (radian or deg if the keyword deg is set)
; ston - S/N ratio or just the 4th column of the lists
; cntr - count rate or just the 5th column of the lists
; ns - number of sources selected
; text - extire rows of selected sources, strings that can then be parsed
; 	later.
; perr - position error in units of arcseconds
;
; revised by wqd, 7/8/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_info,souno,ra,dec,ston,cntr,dif,dis,xp,yp'
print,',[,soufile=,ns=,slow=,flow=,text=,deg=deg,]'
print,',xshift=xshift,yshift=yshift,ashift=ashift,crval=crval,deci=deci,perr=perr'
retall
endif
;
if n_elements(slow) eq 0 then slow=0.
if n_elements(flow) eq 0 then flow=0.
if keyword_set(deg) ne 0 then radeg=1 else radeg=!radeg
if n_elements(soufile) ne 0 then begin 
	openr,un,soufile,/get_lun
	n_source=10000
	souno=strarr(n_source)
	ra=dblarr(n_source)
	dec=dblarr(n_source)
	perr=fltarr(n_source)
	ston=fltarr(n_source)
	cntr=fltarr(n_source)
	dif=fltarr(n_source)
	dis=fltarr(n_source)
	xp=fltarr(n_source)
	yp=fltarr(n_source)
	text=strarr(n_source)
	k=0
	str=''
	while not EOF(un) do begin
           readf,un, str
	   text(k)=str
	   souno(k)=gettok(str,'|')
	   radec=gettok(str,'|')
	   radec=radec+gettok(str,'|')
	   ston(k)=gettok(str,'|')
	   cntr(k)=gettok(str,'|')
	   if ston(k) ge slow and cntr(k) ge flow  then begin
	   dif(k)=gettok(str,'|')
	   dis(k)=gettok(str,'|')
	   xp(k)=gettok(str,'|')
	   yp(k)=gettok(str,'|')
		if keyword_set(deci) then begin
			head=gettok(str,'&')
			ra(k)=gettok(str,'&')
			dec(k)=gettok(str,'&')
			perr(k)=gettok(str,'&')
		endif else begin
	   		stringad,radec,rascale,decscale
	   		ra(k)=rascale & dec(k)=decscale
		endelse
	   k=k+1
	   endif
	endwhile
	free_lun,un
	n_source=k
	if n_source eq 0 then begin
		print,'the file contains no source, return'
		return
	endif
	text=text(0:n_source-1)
	souno=souno(0:n_source-1)
	ra=ra(0:n_source-1)/radeg
	dec=dec(0:n_source-1)/radeg
	perr=perr(0:n_source-1)
	ston=ston(0:n_source-1)
	cntr=cntr(0:n_source-1)
	dif=dif(0:n_source-1)
	dis=dis(0:n_source-1)
	xp=xp(0:n_source-1)
	yp=yp(0:n_source-1)
endif else begin
	print,'Sorry, but the source file name (soufile) is now required'
	return
endelse
if n_elements(crval) ne 0 then begin
	if n_elements(deg) ne 0 then begin
	 trans_dist,crval(0),crval(1),ra,dec,xpp,ypp,/deg
	 shift_xya,xpp,ypp,xshift=xshift,yshift=yshift,ashift=ashift,/noit,/cen
	 trans_loct,xpp,ypp,crval(0),crval(1),ra,dec,/deg
	endif else begin
	 trans_dist,crval(0),crval(1),ra,dec,xpp,ypp
	 shift_xya,xpp,ypp,xshift=xshift,yshift=yshift,ashift=ashift,/noit,/cen
	 trans_loct,xpp,ypp,crval(0),crval(1),ra,dec
	endelse
endif
if !debug eq 2 then stop
return
end
