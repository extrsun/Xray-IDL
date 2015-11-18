pro sou_output_2,infile,outfile,slow=slow,ctoe=ctoe,beta=beta,syserr=syserr
;-
; convert the sou_rates data file into a nice format for final
; output.
; ctoe - conversion from count rate (cts/ks) to energy flux 
; (10^{-14} ergs/s cm^2)
; written by wqd, 11/19/98
;+
if n_params() eq 0 then begin
print,'Calling Seq = sou_output_2,infile,outfile,slow=slow,ctoe=ctoe,beta=beta'
return
endif

if n_elements(ctoe) eq 0 then ctoe=4.
if n_elements(slow) eq 0 then slow=3.5
if n_elements(beta) eq 0 then beta=1.85
source_info,s,sra,sdec,sn,sf,souf=infile,/deg,slow=slow,text=text
ns=n_elements(s)
openw,un,outfile,/get
ns=n_elements(sra)
if n_elements(syserr) eq 0 then syserr=3. ;3" systematic errors
rerr=sqrt(float((strmid(text,92,8))^2+float(strmid(text,102,8))^2)/4.+syserr^2) 
radec_out,ss,sra=sra,sdec=sdec
sf=sf*1.e3
sfml=0.5*(1.+sqrt(1.-4*beta/sn^2))*sf*ctoe
for k=0,ns-1 do begin
 printf,un,'H',k+1,' & ',ss(k),' & ',rerr(k),' & ',sn(k),' & ',sf(k),' & ',sfml(k),' & ','\\',form='(a1,i2,a3,a23,a3,f5.1,a3,f5.1,a3,f7.2,a3,f7.2,a3,a2)'
endfor
free_lun,un
return
end