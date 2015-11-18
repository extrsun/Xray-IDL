function func, x
	r3=0.2
	ss=10^2 ;in units of 10^49
	n0=10^2
	vw=1. ; in units of 2000 km/s
	mwdot=6.1e-5*ss^(5/6)/vw^2/n0^(1/3)
	gamma=1.
	rw=sqrt(20.*vw*mwdot)
	return, r3^3-r2^3-4./9.*(r2/rw)^4/(1.+2./sqrt(3*gamma)*(1.+5*r3^2/sqrt(r3^3-r2^3)/mwdot))^2
end