;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro comb_obs,list,list_out,obfile=obfile
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
	openr,unit,obbfile,/get
	readf,unit,nob,tt
	tint=lonarr(2,nob)
	for n=0,nob-1 do begin
		readf,unit,t1,t2
		tint(0,n)=t1
		tint(1,n)=t2
	endfor
	free_lun,unit
;-------------------------------------------------------------
list_out=list(0)
tvec=list.time
for n = 0,nob-1 do begin
	tabinv,tvec,tint(0,n),indlo
	tabinv,tvec,tint(1,n),indhi
	indhi=long(indhi)
	indlo=long(indlo)+1 < indhi
	if (indlo le indhi) then list_out=[list_out,list(indlo:indhi)]
endfor
list_out=list_out(1:*)
return
end