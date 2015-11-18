pro get_r1, r3v, r1v,r2v
	ss=1 ;in units of 10^49
	n0=10^2
	vw=1. ; in units of 2000 km/s
	mwdot=0.025/vw
	gamma=1.
	rst=12.*ss^(-5./12.)/n0^(1./6.)*sqrt(vw)
b3o=15./sqrt(3.*gamma)/mwdot
b1=4/9.*rst^4
b2=1.+2./sqrt(3*gamma)

nv=n_elements(r3v)
r1v=fltarr(nv)
for k=0,nv-1 do begin
	r3=r3v(k)
	b0=r3^3
	b3=b3o*r3^2
	coeffs=[b0^2-b3^3,-3*b2*b3^2,-(2.*b0*b1+3*b2^2*b3),-b2^3,b1^2]
	r1=fz_roots(coeffs,/double)
;	print,'test',r1	
	sel=where(float(r1) gt 0. and float(r1) lt r3^2)
	print,r1(sel)
	r1=float(r1(sel))
	print,'dif=',total(coeffs*[1.,r1,r1^2,r1^3,r1^4])
	r1v(k)=r1
endfor
r2v=(r3v^3-b1*r1v^2)^(1./3.)
r1v=sqrt(r1v)
stop
return
end