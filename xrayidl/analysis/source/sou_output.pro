pro sou_output,infile,outfile,slow=slow
;-
; convert the sou_rates data file into a nice format for final
; output.
;+
if n_params() eq 0 then begin
print,'Calling Seq = sou_output,infile,outfile,slow=slow'
return
endif
if n_elements(slow) eq 0 then slow=3.5
source_info,s,sra,sdec,sn,sf,sfe,id,sf2,sfe2,souf=infile,/deg,slow=slow,text=text
ns=n_elements(s)
code=strarr(ns)+'0'
code(where(sf2/sfe2 lt slow,nsel))='1'
print,'Number of source detected only in 50\% radius = ',nsel
code(where(sf/sfe lt slow,nsel))='2'
print,'Number of source detected only in 90\% radius = ',nsel
;sel=where((sf/sfe > sf2/sfe2) ge slow)
;sra=sra(sel) 
;sdec=sdec(sel)
;sf=sf(sel)
;sfe=sfe(sel)
;sf2=sf2(sel)
;sfe2=sfe2(sel)
;id=id(sel)
;text=text(sel)
; sn is the maximum in the sou_rate file
openw,un,outfile,/get
ns=n_elements(sra)
rerr=sqrt(float((strmid(text,134,6))^2+float(strmid(text,144,6))^2)/4.+16.) ;4" systematic errors
radec_out,ss,sra=sra,sdec=sdec
for k=0,ns-1 do begin
	if id(k) eq 1 then code(k)=code(k)+'e' else code(k)=code(k)+'~'
 printf,un,'H',k+1,' & ',ss(k),' & ',rerr(k),' & ',sf2(k),'$\pm$',sfe2(k),' & ',sf(k),'$\pm$',sfe(k),' & ',code(k),' & ','\\',form='(a1,i2,a3,a23,a3,f5.1,a3,2(f7.2,a5,f4.2,a3),a3,a3,a2)'
endfor
free_lun,un
return
end