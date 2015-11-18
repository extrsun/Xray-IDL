pro poisson,mean,np,p
;compute probabilties of obtaining X > n where n=0,1,2..
if (n_params(0) eq 0 ) then begin
print,' POSSION mean np prob '
print,' compute np poisson probabilities (prob) that X > n'
retall
end
nx=findgen(np)
prob=dblarr(np)
p=dblarr(np)
mfac = exp(-1.*mean)
p(0) = mfac
prob(0) = 1.-p(0)
sum =  p(0)
for i=1,np-1 do begin
 if i le 11 then begin
	p(i) = p(i-1)*mean/float(i)
	sum = sum+p(i)
 	prob(i)= 1.0d0 - sum
 endif
endfor
;forprint,nx(0:np-1),p(0:np-1),prob(0:np-1)
end

