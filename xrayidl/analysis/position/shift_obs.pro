pro shift_obs,list,image_t,image_b,refx,refy,shiftx,shifty $
,obbfile=obbfile,obsfile=obsfile,tblock=tblock,chn=chn
; +
; calculating shifts of images of individual time segments, by comparing
; cetroids of sources with their reference positions.
; list - count list
; image_t - exposure map
; image_b - background count map
; tblock - block used for constructing image_t and image_b
; refx, refy - reference position in pixels relative to the image center
; shiftx,shifty - calculated shifts in pixels relative to the reference
;		position
; obbfile - file name of the input file containing time segments and
;	their observing duration from back_obs.pro
; obsfile - output file name to be used list_shift.pro
; chn - choice for the ML fitting algorithm: chn=1 for x and y fitting only.
; 	chn=2 (def) fits both the position and the Gaussian size.
;
; written by wqd, 9/16/97
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - shift_obs,list,image_t,image_b,refx,refy'
print,',shiftx,shifty,obbfile=obbfile,obsfile=obsfile,tblock=tblock,chn=chn'
return
endif
if n_elements(chn) eq 0 then chn=2
if n_elements(tblock) eq 0 then tblock=64
if n_elements(obfile) eq 0 then obbfile=!seq_no+'_obbtime.dat'
	openr,unit,obbfile,/get
	readf,unit,nob,tt
	tint=lonarr(2,nob)
	tobv=lonarr(nob)
	delfbv=fltarr(nob)
	for n=0,nob-1 do begin
		readf,unit,t1,t2,tob,delfb
		tint(0,n)=t1
		tint(1,n)=t2
		tobv(n)=tob
		delfbv(n)=delfb
	endfor
	free_lun,unit
;-------------------------------------------------------------
tfb=image_b/tt
tnorm=image_t/tt 
delfbv=delfbv*(tblock/120.)^2   ; a factor for converting cts/s arcmin^2
				; to cts/s bin of the background map

ns=n_elements(refx)
shiftx=fltarr(nob)
shifty=shiftx
shifta=shiftx
if chn eq 2 then ssea=fltarr(ns,3,nob) else ssea=fltarr(ns,2,nob)
;
; get source pixel positions
tvec=list.time
for n = 0,nob-1 do begin
	tabinv,tvec,tint(0,n),indlo
	tabinv,tvec,tint(1,n),indhi
	indhi=long(indhi)
	indlo=long(indlo)+1 < indhi
	ls=list(indlo:indhi)
	bc_ob=(tfb+delfbv(n)*tnorm)*tobv(n) ;background count map in the seg.
	t_ob=tobv(n)*tnorm ;exposure map in the seg.
	mle_anal,ls,t_ob,bc_ob,refx,refy,xmv,ymv,sse,tblock=tblock,chn=chn $
		,snr,cntr
	print,'snr,cntr = ',snr,cntr
	ssea(*,*,n)=sse
  if ns ge 2 then begin
	offset_fit,xmv,ymv,refx,refy,1./(sse(*,0)^2+sse(*,1)^2),val
	shiftx(n)=val(0)
	shifty(n)=val(1)
	shifta(n)=val(2)
	print,'shiftxya = ',shiftx(n),shifty(n),shifta(n)
  endif else begin
	shiftx(n)=refx-xmv & shifty(n)=refy-ymv
	print,'nob shiftxya, sse = ',n, shiftx(n),shifty(n),sse
  endelse
if !debug eq 3 then stop
endfor
;-------------------------------------------------------------
if n_elements(obsfile) eq 0 then obsfile=!seq_no+'_obstime.dat'
openw,unit,obsfile,/get
printf,unit,nob
for n=0,nob-1 do printf,unit,tint(0,n),tint(1,n) $
	,shiftx(n),shifty(n),shifta(n)
free_lun,unit
end


