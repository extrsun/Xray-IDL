pro back_obs,list,xmin,ymin,image_ts $
,obfile=obfile,obbfile=obbfile,block=block
;+
; list - count list
; image_ts - source-removed exposure map used for filtering the list
; xmin, ymin - the lower left pixel coordinates of the image_ts
; block - block of image_ts
; obfile - file name of the input file containing time segments
; obbfile - the name of the output file including the background excess
;	relative to the average of the observation.
;	The file to be used by shift_obs.pro.
; written by wqd 9/16/97
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - back_obs,list,xmin,ymin,image_ts'
print,',obfile=obfile,obbfile=obbfile,block=block'
return
endif
if n_elements(block) eq 0 then block=8 ;image block
if n_elements(obfile) eq 0 then obfile=!seq_no+'_obtime.dat'
openr,unit,obfile,/get
readf,unit,nob,tt
tint=lonarr(2,nob)
tobv=lonarr(nob)
for n=0,nob-1 do begin
	readf,unit,nn,t1,t2,tob
	tint(0,n)=t1
	tint(1,n)=t2
	tobv(n)=tob
endfor
free_lun,unit
;-------------------------------------------------------------
sz=size(image_ts)
dim=sz(1)
delfb=fltarr(nob)
fac=(120./block)^2/total(image_ts)
tvec=list.time

list_image,list,xmin,ymin,image,dim,block=block,filter=image_ts
tc=total(image) ;total counts of the filtered total image

for n = 0,nob-1 do begin
	tabinv,tvec,tint(0,n),indlo
	tabinv,tvec,tint(1,n),indhi
	indhi=long(indhi)
	indlo=long(indlo)+1 < indhi
	ls=list(indlo:indhi)
	list_image,ls,xmin,ymin,image,dim,block=block,filter=image_ts
	delc=total(image)-tc*tobv(n)/tt ;excess counts
	delfb(n)=delc*tt/tobv(n)*fac ;in units of counts/s arcmin^2
	print,'delfb = ',delfb(n)
endfor
print,'total(delfb*tobv)= ',total(delfb*tobv)
;-------------------------------------------------------------
if n_elements(obbfile) eq 0 then obbfile=!seq_no+'_obbtime.dat'
openw,unit,obbfile,/get
printf,unit,nob,tt
for n=0,nob-1 do printf,unit,tint(0,n),tint(1,n),tobv(n),delfb(n)
free_lun,unit
end


