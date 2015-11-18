pro poisson_inv,expectco,sig,valuea,interp=interp,gaussth=gaussth
;+
; Program:
; calculate the first integer(s) that exceeds the significance (according
; to Poisson statistics) with given expected value(s).
; written by wqd, Sept, 13, 2003
;-
if n_params() lt 2 then begin
   print,' syntax: poisson_inv,expectc,sig,value, where'
   print,'         o EXPECTCO is an array of expected values"'
   print,'         o SIG - the significance (0-1)'
   print,'         o VALUEA - output upper integer(s)'
   print,'         o INTERP - if set, the output is interplated'
   print,'         o GAUSSTH - threshold to use the Gaussian approximation'
   return
endif
sig=double(sig)
if max(sig) eq 0 or max(sig) eq 1 then begin
   print,'max(sig) eq 1! May be double precision for the signficance is needed'
   return
endif
if n_elements(gaussth) eq 0 then begin
    gaussth=15.0d
    print,'For expected values greater than ',gaussth,', Gaussian approximation is used!'
endif 
sel=where(expectco gt 0. and expectco lt gaussth,nexpect)
gsel=where(expectco ge gaussth,ngsel)
if nexpect eq 0 and ngsel eq 0 then begin
    print,'no non-zero values in expetco!'
    return
endif
valuea=expectco*0.
if nexpect ne 0 then begin
    expectc=expectco(sel)
    expect=fltarr(nexpect)+expectc
    value=fltarr(nexpect)

    for k=0L,nexpect-1 do begin
	n=fix(expect(k))+1
	sig1=poisson_sig(expect(k),n)
	n=n+1
	sig2=poisson_sig(expect(k),n)
	while sig2(0) lt sig do begin
		n=n+1
		sig1=sig2
		sig2=poisson_sig(expect(k),n)
	endwhile
	if keyword_set(interp) then begin
            linterp,[sig1,sig2],[n-1,n],sig,val
            value(k)=val
        endif else value(k)=n
    endfor
    valuea(sel)=value
endif
;now for bins that can be approximated with Gauassian 
if ngsel ne 0 then begin
    gaussvalue=gauss_cvf(1.0-sig)
    valuea(gsel)=gaussvalue*sqrt(expectco(gsel))+expectco(gsel)
endif
return
end
