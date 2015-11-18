pro poisson_inv,expect,sig,value

sig1=exp(-expect)
n=1
sig2=poison_sig(expect,n))
while sig2 lt sig do begin
	n=n+1
	sig1=sig2
	sig2=poison_sig(expect,n))
endwhile
linterp,[sig1,sig2],[n,n-1],sig,value
return
end