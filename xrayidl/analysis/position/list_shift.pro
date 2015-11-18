pro list_shift,list,list_out,obsfile=obsfile,noobs=noobs $
	,xshift=xshift,yshift=yshift,ashift=ashift,shiftlimit=shiftlimit
;-
; shift x and y positions of individual counts in a list 
; 
; list - input list 
; list_out - output list after the shift corrections
; obsfile - the name of the file containing time segments for different
;		shifts, provided by shift_obs.pro
; xshift,yshift,ashift - ADDITIONAL global shifts uniformly applied for
;			all counts
; noobs - if set, only the global shifts will be made, but no time-dependent 
; 	shifts will carried out
; shiftlimit - only shifts greater than this limit will be made 
;	(def = 5 pixels)
; writen by wqd, Dec. 10, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - list_shift,list,list_out,obsfile=obsfile,noobs=noobs'
print,',xshift=xshift,yshift=yshift,ashift=ashift,shiftlimit=shiftlimit'
return
endif

if keyword_set(noobs) then begin
	list_out=list
	x=list.x
	y=list.y
	shift_xya,x,y,xsh=xshift,yshift=yshift,ash=ashift
	list_out.x=x
	list_out.y=y
	return
endif
;
if n_elements(xshift) eq 0 then xshift=0.
if n_elements(yshift) eq 0 then yshift=0.
if n_elements(ashift) eq 0 then ashift=0.
if n_elements(shiftlimit) eq 0 then shiftlimit=5.
;list_out=list(sort(t))
list_out=list
tvec=list_out.time ; the list of a rosat observation is presorted

if n_elements(obsfile) eq 0 then obsfile=!seq_no+'_obstime.dat'
openr,unit,obsfile,/get
readf,unit,nob
tint=lonarr(2,nob)
sh=fltarr(3,nob)
t1=0L
t2=t1
for n=0,nob-1 do begin
	readf,unit,t1,t2,shiftx,shifty,shifta
	tint(0,n)=t1
	tint(1,n)=t2
	if shiftx^2+shifty^2 gt shiftlimit^2 then begin
		sh(0,n)=shiftx 
		sh(1,n)=shifty
		sh(2,n)=shifta*(180./!pi)
	endif
endfor
free_lun,unit

;tabinv_m,tvec,tint(1,*),indend
;indend=long(indend)
;indstart=[0,indend(0:nob-2)+1]

x=list_out.x
y=list_out.y
for n=0,nob-1 do begin
	tabinv,tvec,tint(0,n),indlo
	tabinv,tvec,tint(1,n),indhi
	indhi=long(indhi)
	indlo=long(indlo)+1 < indhi
	xx=x(indlo:indhi)
	yy=y(indlo:indhi)
	shift_xya,xx,yy,xshift=xshift+sh(0,n) $
		,yshift=yshift+sh(1,n),ashift=ashift+sh(2,n)
	x(indlo:indhi)=xx
	y(indlo:indhi)=yy
endfor

list_out.x=x
list_out.y=y
end


