;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro comb_obs,list,sel,obfile=obfile
;-
; 
; writen by wqd, april 16, 1996
;+
if n_params() eq 0 then begin
print,'ml_anal,list,cra,cdec,image_t,image_b,tblock=tblock,'
print,' radius=radius,threshold=threshold,infile=infile,outfile=outfile'
print,' ,append=append,blow=blow,bhigh=bhigh,sfrac=sfrac,chn=chn'
print,' ,slow=slow,flow=flow,sigma=sigma,rc=rc,verb=verb'
return
endif
;
if n_elements(obfile) eq 0 then obfile=!seq_no+'_gtidif.dat'
	openr,unit,obfile,/get
	readf,unit,nob,tt
	tint=lonarr(2,nob)
	for n=0,nob-1 do begin
		readf,unit,t1,t2
		tint(0,n)=t1
		tint(1,n)=t2
	endfor
	free_lun,unit
;-------------------------------------------------------------
sel=[999]
tvec=list.time
	tabinv_m,tvec,tint(0,*),indlo
	tabinv_m,tvec,tint(1,*),indhi
	indhi=long(indhi)
	indlo=long(indlo)+1 < indhi
stop
for n = 0,nob-1 do begin
	if (indlo(n) le indhi(n)) then sel=[sel,lindgen(indhi(n)-indlo(n)+1)+indlo(n)]
endfor
sel=sel(1:*)
stop
return
end