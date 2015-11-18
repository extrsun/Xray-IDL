pro feintegral,gamma,e0,alpha,nterm,sum
if n_params(0) eq 0 then begin
 print,'feintegral,gamma,e0,alpha,nterm,sum'
 print,' Compute the integral int{E^-gamma (1-exp[alpha(E/e0)^-3.1])}'
 print,' by expanding 1-exp(..) up to nterm terms '
 retall
end
fact=1.
sum=0.0
for k=0l,nterm-1 do begin
term=0.
n=float(k+1)
eveodd=k-2*(k/2) 
if eveodd eq 0 then sgn=1. else sgn=-1.
fact=fact*n
alphan=alpha^n
denom=3.1*n + gamma -1.
top=e0^(1.-gamma)
term=sgn*alphan*top/fact/denom
sum=term+sum
;print,k+1,term,sum
endfor
return
end
