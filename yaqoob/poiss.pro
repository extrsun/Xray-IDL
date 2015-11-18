pro poiss,mean,k,prob
if n_params(0) eq 0 then begin
 print,'poiss,mean,k,prob'
 print,'Input floating array mean and return the Poisson probability of'
 print,'obtaining greater than or equal to k'
 retall
end
x=indgen(k+1)
factorial,x,xf
;forprint,x,xf
probp=mean & probp=0. 
for j=0l,k-1 do begin
 probp=probp+((mean^float(j))/xf(j))
endfor
probp=probp*exp(-1.*mean) & probp=1.-probp
sigma=sqrt(mean)
probg=(float(k)-mean)/sigma
probg=1.-gaussint(probg)
meanmax=max(mean)
prob=probp
if meanmax gt 20. then begin 
;prob(where(mean le 20.))=probp(where(mean le 20.))
prob(where(mean gt 20.))=probg(where(mean gt 20.))
endif
wz=where((prob lt 0.0),nwz)
if nwz gt 0 then prob(where(prob lt 0.))=0.
return
end
