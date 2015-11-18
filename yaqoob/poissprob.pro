pro poissprob,mu,n,prob
if n_params(0) eq 0 then begin
 print,'poissprob,mu,n,prob'
 print,'Compute Poisson probability'
 retall
end
if abs(mu) gt 87. then begin
 prob=0.0
 return
endif
xn=double(n) & xmu=double(mu) 
if n le 20. then begin
	lfn=alog(factorial(n))
endif else begin
 	lfn=xn*(alog(xn)-1.0d0) + alog(sqrt(2.*!pi*xn))
endelse
lp=xn*alog(xmu) -lfn -xmu
if lp lt -87.0d0 then begin
 prob=0.0
endif else if lp gt 87.0d0 then begin
 prob=1.e38
endif else begin
 prob=exp(lp)
endelse
return
end
