pro poisig_v,expect,kdupo,sig,nolog=nolog
;+
; calculate the significance of count deviations from expect values
; When the expect value is greater than gauth (def=50), gaussian approximation
; is used.
; expect - vector containing the expected values
; kdupo - the number of observed counts (integers)
; sig - assumulated probability for the number of counts to be greater than,
; or equal to, kdupo;
; written by wqd, April, 21, 2002
;  "equal to" is added , Nov. 22, 2003
;-
if n_params() eq 0 then begin
print,'Calling Seq: poisig_v,expect,kdupo,sig,nolog=nolog'
return
endif
;
gauth=36d
nbin=n_elements(expect) > n_elements(kdupo)
expectd=dblarr(nbin)+double(expect) ;make sure the same dimension
;kdup=lonarr(nbin)+kdupo-1
kdup=lonarr(nbin)+kdupo
sig=dblarr(nbin)+1.
ss=where(expectd gt gauth or kdup gt gauth,nss)
if nss ne 0 then begin
	sigma=(kdup(ss)-expectd(ss))/sqrt(expectd(ss)) ;< 8. 
		;limit that erroorf can handle, corresponding to sig=-15.2142
	sig(ss)=0.5*(1.-errorf(sigma/sqrt(2.)))
	;negative sigma gives a negative output from errorf
endif 
if nss eq nbin then goto,done
;gss=lindgen(nbin)
;if nss ne 0 then remove,ss,gss
ss=where(expectd le gauth and  kdup le gauth and expectd gt 0.0d,nss)
if nss eq 0 then goto,done
prob=exp(-expectd(ss))
sig(ss)=1.0-prob
loci=where(kdup(ss) gt 0,nloci)
if nloci eq 0 then goto,done
ssl=ss(loci)
counts=kdup(ssl)
mc=max(counts)
for n=1L,mc do begin	
	prob(loci)=prob(loci)*expectd(ssl)/double(n) 
	sig(ssl)=sig(ssl)-prob(loci)          
if !debug eq 1 then stop      
	sel=where(counts eq n,nsel)
	if nsel ne 0 and n ne mc then remove,sel,loci,ssl,counts
endfor 
done: 
if keyword_set(nolog) eq 0 then sig=alog10(sig > 1.e-20)
return
end
