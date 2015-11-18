pro sou_struct_out,struct_s,text,outfile=outfile,bsym=bsym,tagsel=tagsel,format=format,dig=dig,nsv=nsv,htext=htext,comment=comment,verbose=verbose
;+
; Output the source structure into an array of text and optionally into a file
;
; struct_s - source structure
; text - output array of source text o
; outfile - output file name
; bsym - symbol that separates two entry in the source text
; tagsel - a vector of selected tag names in the source structure. 
;		if not given, all tags in the structure will be used
; format - output source text format in the fortran format. 
;	if not given, the formatv will be searched for the given tag names
;	as listed in tagv
; dig - the number of RA second decimal point, if given, IAUID will be 
;	recalculated
; nsv - vector contains the source numbers
; htext - string vector containing the tag labels
; comments - add the comment column in the output
; verbose - if set, the output text will be printed on screen
;
; writen by wqd, 12/30/2001
;-
if n_params() eq 0 then begin
print,'Calling procedure - '
print,'sou_struct_out,struct_s,text,outfile=outfile,bsym=bsym,tagsel=tagsel'
print,',format=format,dig=dig,noiauname=noiauname,verb=verb,nsv=nsv,comment=comment,verbose=verbose'
return
endif
;
if n_elements(bsym) eq 0 then bsym=' | '
tagv=['IAUID','SN','RA','DEC','PERR','SNR','CNTR','CNTRE','CNTRB','CNTRBE' $ ;1
 ,'CNTRB1','CNTRB1E','CNTRB2','CNTRB2E','CNTRB3','CNTRB3E' $ ;temporary ;2
 ,'CNTRB1','CNTRBE1','CNTRB2','CNTRBE2','CNTRB3','CNTRBE3','CNTRB4','CNTRBE4','CNTRB5','CNTRBE5' $ ;may be expanded to higher numbers ;3
 ,'MCNTR','OFFAXIS','OBS_ID','MSNR','MSNRB','ACNTR','ACNTRE','ACHIS','ANDUP','ACNTRB1','ACNTRB1E','ACNTRB2' $ ;4
 ,'ACNTRB2E','ACNTRB3','ACNTRB3E','CHIS','NDF','PROB','SRADIUS' $
 ,'COUNT','BCOUNT','EXPT','PSYSERR','CNTRM','CNTREM','NDUP','MOBDUP','SDB' $
 ,'HR','HRE','HR1','HR1E','HR2','HR2E' $
 ,'NXP','NYP','VSR','VBC','VPROB','VSC','VFLAG'] ;variable analysis
FORMATV=['(a19)','(I4)','(f10.5)','(f10.5)','(f5.2)','(f6.1)','(e9.2)','(e9.2)','(e9.2)','(e9.2)' $ ;1
 ,'(e9.2)','(e9.2)','(e9.2)','(e9.2)','(e9.2)','(e9.2)' $ ;temporary 2
 ,'(e9.2)','(e9.2)','(e9.2)','(e9.2)','(e9.2)','(e9.2)','(e9.2)','(e9.2)','(e9.2)','(e9.2)'$ ;3
 ,'(e9.2)','(f6.2)','(I6)','(f6.1)','(f6.1)','(e9.2)','(e9.2)','(I2)','(e9.2)','(e9.2)','(e9.2)','(e9.2)' $ ;4
 ,'(e8.1)','(e9.2)','(e9.2)','(e9.2)','(I3)','(f5.1)','(f5.1)' $
 ,'(I7)','(e7.1)','(e7.1)','(e7.1)','(e9.2)','(e9.2)','(I3)','(I3)','(a16)' $
 ,'(f7.3)','(f7.3)','(f7.3)','(f7.3)','(f7.3)','(f7.3)' $
 ,'(I6)','(I6)','(f7.2)','(f6.1)','(f6.1)','(f6.1)','(a10)'] ;variable analysis
tagname=tag_names(struct_s)
if n_elements(tagsel) eq 0 then begin
	tagsel=tagname
	ntag=n_tags(struct_s)
	tagind=indgen(ntag)
endif else begin
	ntag=n_elements(tagsel)
	match,tagname,strupcase(tagsel),tagind,tagselind,count=ntag_match
	if ntag_match eq 0 then stop,'no match of the tagsel!!!'
	if ntag_match ne ntag then stop,'no all tags are matched!!!'
	;tagind is not necessarily in the order
	ss=sort(tagselind)
	tagind=tagind(ss) 
	tagname=tagname(tagind)
endelse
if n_elements(format) eq 0 then begin
	match,tagv,strupcase(tagname),find,tagnameind,count=nmatch
	find=find(sort(tagnameind))
	format=formatv(find)
endif

ns=n_elements(struct_s)
text=strarr(ns)
if n_elements(nsv) ne 0 then $
	text=text+string(strtrim(nsv,2),'(A5)')+bsym
htext=''
if n_elements(dig) ne 0 then begin
	radec_out,struct_s.ra,struct_s.dec,dig=dig,iauname=iauname
	text=text+iauname+bsym
	htext=htext+'IAUID'+bsym
	klo=1
endif else klo=0
	
for k=klo,ntag-1 do begin
	text=text+string(strtrim(struct_s.(tagind(k)),2),format(k))+bsym
	htext=htext+tagsel(k)+bsym
endfor

if keyword_set(comment) then begin
	htext=htext+'Comments'+' \\'
	text=text+' \\'
endif
if keyword_set(verbose) ne 0 then begin
	print,htext
	for k=0L,ns-1 do print,text(k)
endif
if n_elements(outfile) ne 0 then begin
	openw,un,outfile,/get
	printf,un,htext
	for k=0L,ns-1 do printf,un,text(k)
	free_lun,un
endif
return
end
